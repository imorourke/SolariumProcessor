#include <assert.h>
#include <cerrno>
#include <ctime>
#include <errno.h>
#include <fcntl.h>
#include <fuse3/fuse.h>
#include <limits>
#include <memory>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "cbfs.h"

#include <iomanip>
#include <iostream>
#include <mutex>
#include <shared_mutex>
#include <unordered_map>

#ifdef WIN32
#define fuse_file_info_t fuse_file_info
#define fuse_filler_t fuse_fill_dir_t
#define fuse_statfs_t fuse_statvfs
#else

#ifdef __APPLE__
#define fuse_stat fuse_darwin_attr
#define fuse_off_t off_t
#define fuse_mode_t mode_t
#define fuse_statvfs statvfs
#define fuse_file_info_t file_info
#define fuse_timespec timespec
#define fuse_filler_t fuse_darwin_fill_dir_t
#define fuse_statfs_t statfs
#else
#define fuse_stat stat
#define fuse_off_t off_t
#define fuse_mode_t mode_t
#define fuse_statvfs statvfs
#define fuse_file_info_t file_info
#define fuse_timespec timespec
#define fuse_filler_t fuse_fill_dir_t
#define fuse_statfs_t fuse_statvfs
#endif

#endif

struct cbfs_error {
    CbFsResult result;

    int get_return_code() const {
        switch (result) {
        case CbFsResult::Success:
            return 0;
            break;
        case CbFsResult::EntryNotDirectory:
            return -ENOTDIR;
        case CbFsResult::EntryNotFile:
        case CbFsResult::EntryNotFound:
        case CbFsResult::InvalidEntry:
            return -ENOENT;
        case CbFsResult::NoSpace:
            return -ENOSPC;
        case CbFsResult::DuplicateName:
            return -EEXIST;
        case CbFsResult::InvalidName:
            return -ENAMETOOLONG;
        default:
            return -ENOSYS;
        }
    }

    static void check_return(const CbFsResult& result) {
        if (result != CbFsResult::Success) {
            throw cbfs_error{ .result = result };
        }
    }
};

struct CbFuseState {
    CbFs* fs{};
    uint16_t block_size{};
    std::shared_mutex lock{};
    const char* base_file{};
    bool read_only{ false };
    bool save_gzip{ false };
    bool save_sparse{ false };
    bool save_zeroed{ false };
    bool print_enabled{ false };
    const char* debug_name{ nullptr };
    std::unordered_map<uint16_t, fuse_mode_t> current_modes{};

    bool save_fs() {
        if (base_file != nullptr && !read_only) {
            return cbfs_save(
                       fs,
                       base_file,
                       CbFsSaveOption{
                           .gzip = save_gzip,
                           .sparse = save_sparse,
                           .zero_unused = save_zeroed,
                       }
                   ) == CbFsResult::Success;
        } else {
            return true;
        }
    }

    CbFsEntry get_entry(const char* path) const {
        CbFsEntry entry{};
        cbfs_error::check_return(cbfs_get_entry_by_path(fs, path, &entry));
        return entry;
    }

    CbFsEntry get_entry(uint16_t id) const {
        CbFsEntry entry{};
        cbfs_error::check_return(cbfs_get_entry(fs, id, &entry));
        return entry;
    }

    bool can_print_debug() const { return print_enabled; }

    bool can_print_debug(const char* path) const {
        return print_enabled && (debug_name == nullptr || (path != nullptr && strcmp(path, debug_name) == 0));
    }

    static CbFuseState* get_instance() { return static_cast<CbFuseState*>(fuse_get_context()->private_data); }
};

static void cbfs_fuse_destroy(void* private_data) {
    auto state = static_cast<CbFuseState*>(private_data);
    if (state->can_print_debug()) {
        std::cout << "destroy()\n";
    }

    if (state != nullptr) {
        state->save_fs();
        cbfs_destroy(state->fs);

        delete state;
    }
}

static fuse_timespec millis_to_timespec(int64_t millis) {
    return fuse_timespec{
        .tv_sec = millis / 1000,
        .tv_nsec = (millis % 1000) * 1000000,
    };
}

static int64_t timespec_to_millis(const fuse_timespec& ts) { return ts.tv_sec * 1000 + ts.tv_nsec / 1000000; }

static struct fuse_stat cbfs_util_getstat(
    const CbFuseState& state,
    const CbFsEntry& entry
) {
    struct fuse_context* ctx = fuse_get_context();
    struct fuse_stat stbuf{};

    fuse_mode_t mode_val{};

#ifdef WIN32
    constexpr fuse_mode_t FILE_MODE = S_IFREG | 0777;
    constexpr fuse_mode_t FOLDER_MODE = S_IFDIR | 0777;
#else
    constexpr fuse_mode_t FILE_MODE = S_IFREG | 0644;
    constexpr fuse_mode_t FOLDER_MODE = S_IFDIR | 0755;
#endif

    if (entry.entry_type == CbFsEntryType::Directory) {
        mode_val = FOLDER_MODE;
    } else {
        mode_val = FILE_MODE;
    }

    constexpr uint32_t DEFAULT_BLOCK_SIZE = 512;
    const uint32_t block_count = entry.size_bytes / DEFAULT_BLOCK_SIZE + ((entry.size_bytes == DEFAULT_BLOCK_SIZE) ? 0 : 1);
    const auto time_val = millis_to_timespec(cbfs_time_to_millis(&entry.last_time));

#ifdef __APPLE__
    stbuf.ino = entry.entry_id;
    stbuf.blksize = state.block_size;
    stbuf.blocks = block_count;
    stbuf.size = entry.size_bytes;
    stbuf.atimespec = time_val;
    stbuf.mtimespec = time_val;
    stbuf.ctimespec = time_val;
    stbuf.uid = ctx->uid;
    stbuf.gid = ctx->gid;
    stbuf.nlink = 1;
    stbuf.mode = mode_val;
#else
    stbuf.st_ino = entry.entry_id;
    stbuf.st_blksize = state.block_size;
    stbuf.st_blocks = block_count;
    stbuf.st_size = entry.size_bytes;
    stbuf.st_atim = time_val;
    stbuf.st_mtim = time_val;
    stbuf.st_ctim = time_val;
    stbuf.st_uid = ctx->uid;
    stbuf.st_gid = ctx->gid;
    stbuf.st_nlink = 1;
    stbuf.st_mode = mode_val;
#endif

    return stbuf;
}

static struct fuse_stat util_getstat(
    const CbFuseState& state,
    const char* path
) {
    return cbfs_util_getstat(state, state.get_entry(path));
}

static struct fuse_stat util_getstat(
    const CbFuseState& state,
    uint16_t entry_val
) {
    return cbfs_util_getstat(state, state.get_entry(entry_val));
}

static int cbfs_fuse_getattr(
    const char* path,
    struct fuse_stat* stbuf,
    struct fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::shared_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "getattr(" << path << ", " << fi << ", " << ((fi != nullptr) ? fi->fh : 0) << ")\n";
    }

    try {
        *stbuf = util_getstat(*state, path);

        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_readdir(
    const char* path,
    void* buf,
    fuse_filler_t filler,
    fuse_off_t offset,
    struct fuse_file_info* fi,
    fuse_readdir_flags
) {
    const auto state = CbFuseState::get_instance();
    std::shared_lock lk(state->lock);

    if (state->can_print_debug(path)) {
        std::cout << "readdir(" << path << ")\n";
    }

    CbFsDirectoryList* entries{};

    try {
        const CbFsEntry entry = state->get_entry(fi->fh);

        if (entry.entry_type != CbFsEntryType::Directory) {
            return -ENOENT;
        }

        const auto add_entry = [&buf, &filler](const struct fuse_stat* val, const char* name) {
            if (0 != filler(buf, name, val, 0, FUSE_FILL_DIR_PLUS)) {
                throw cbfs_error{ .result = CbFsResult::UnknownError };
            }
        };

        cbfs_error::check_return(cbfs_read_dir(state->fs, entry.entry_id, &entries));
        if (entries != nullptr) {
            const uint32_t entry_size = cbfs_read_dir_size(entries);
            for (size_t i = offset; i < entry_size + 2; ++i) {
                if (i == 0) {
                    const auto val = util_getstat(*state, entry.entry_id);
                    add_entry(&val, ".");
                } else if (i == 1) {
                    uint16_t parent_id = 0;
                    cbfs_error::check_return(cbfs_get_parent_node(state->fs, entry.entry_id, &parent_id));
                    if (parent_id != 0) {
                        const auto val = util_getstat(*state, parent_id);
                        add_entry(&val, "..");
                    } else {
                        add_entry(nullptr, "..");
                    }
                } else {
                    CbFsEntry entry{};
                    cbfs_error::check_return(cbfs_read_dir_entry(entries, i - 2, &entry));
                    const auto val = util_getstat(*state, entry.entry_id);
                    add_entry(&val, entry.name);
                }
            }
            cbfs_read_dir_destroy(entries);
            entries = nullptr;
        } else {
            throw cbfs_error{ .result = CbFsResult::InvalidEntry };
        }
        return 0;
    } catch (const cbfs_error& err) {
        if (entries != nullptr) {
            cbfs_read_dir_destroy(entries);
        }
        return err.get_return_code();
    }
}

static int cbfs_fuse_statfs(
    const char* path,
    struct fuse_statfs_t* statfs
) {
    const auto state = CbFuseState::get_instance();
    std::shared_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "statfs(" << path << ")\n";
    }

    try {
        CbFsStats fs_stats{};
        cbfs_error::check_return(cbfs_get_stats(state->fs, &fs_stats));

#ifndef __APPLE__
        statfs->f_namemax = FILENAME_MAX;
        statfs->f_frsize = fs_stats.block_size;
#endif
        statfs->f_bsize = fs_stats.block_size;

        statfs->f_blocks = fs_stats.num_blocks;
        statfs->f_bfree = fs_stats.free_blocks;
        statfs->f_bavail = fs_stats.free_blocks;

        statfs->f_files = fs_stats.num_blocks;
        statfs->f_ffree = fs_stats.free_blocks;
#ifndef __APPLE__
        statfs->f_favail = fs_stats.free_blocks;
#endif

#ifndef __APPLE__
        statfs->f_fsid = 0xA80E83BC;
        statfs->f_flag = 0;
#endif

        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_open(
    const char* path,
    struct fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::shared_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "open(" << path << ", " << std::hex << fi->flags << ")\n";
    }

    try {
        fi->fh = state->get_entry(path).entry_id;
        return 0;
    } catch (const cbfs_error& err) {
        std::cout << "Open error: " << path << '\n';
        return err.get_return_code();
    }
}

static int cbfs_fuse_opendir(
    const char* path,
    struct fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::shared_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "opendir(" << path << ")\n";
    }

    try {
        CbFsEntry entry = state->get_entry(path);

        fi->fh = entry.entry_id;
#ifndef WIN32
        fi->cache_readdir = true;
#endif

        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_read(
    const char* path,
    char* buf,
    size_t size,
    fuse_off_t offset,
    struct fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::shared_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "read(" << path << ")\n";
    }

    try {
        if (size > std::numeric_limits<int32_t>::max()) {
            return -ENOSYS;
        }

        uint32_t size_u32 = static_cast<uint32_t>(size);

        cbfs_error::check_return(
            cbfs_read_entry_data(state->fs, fi->fh, static_cast<uint32_t>(offset), reinterpret_cast<uint8_t*>(buf), &size_u32)
        );

        return size_u32;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_mkdir(
    const char* path,
    fuse_mode_t
) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "mkdir(" << path << ")\n";
    }

    try {
        cbfs_error::check_return(cbfs_create_entry(state->fs, path, CbFsEntryType::Directory, nullptr, false));
        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_rename(
    const char* old_path,
    const char* new_path,
    unsigned int
) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(old_path)) {
        std::cout << "mkdir(" << old_path << ", " << new_path << ")\n";
    }
    try {
        cbfs_error::check_return(cbfs_rename_entry(state->fs, old_path, new_path, true));
        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_unlink(const char* path) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "Unlinking " << path << std::endl;
    }

    try {
        cbfs_error::check_return(cbfs_remove_entry(state->fs, path, CbFsEntryType::File));
        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_rmdir(const char* path) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "rmdir(" << path << ")\n";
    }

    try {
        cbfs_error::check_return(cbfs_remove_entry(state->fs, path, CbFsEntryType::Directory));
        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_fsync(
    const char* path,
    int,
    struct fuse_file_info*
) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "fsync(" << path << ")\n";
    }

    if (!state->save_fs()) {
        return -ENOSYS;
    } else {
        return 0;
    }
}

static int cbfs_fuse_create(
    const char* path,
    fuse_mode_t mode,
    struct fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "create(" << path << ", " << std::hex << mode << ", " << std::hex << fi->flags << ")\n";
    }

    const bool can_truncate = (mode & (O_CREAT | O_TRUNC)) == (O_CREAT | O_TRUNC);

    try {
        CbFsEntry entry{};
        cbfs_error::check_return(cbfs_create_entry(state->fs, path, CbFsEntryType::File, &entry, can_truncate));
        fi->fh = entry.entry_id;
        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_mknod(
    const char* path,
    fuse_mode_t mode,
    dev_t
) {
    struct fuse_file_info fi{};
    return cbfs_fuse_create(path, mode, &fi);
}

static int cbfs_fuse_write(
    const char* path,
    const char* data,
    size_t size,
    fuse_off_t offset,
    struct fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "write(" << path << ", " << fi << ")\n";
    }

    try {
        uint16_t entry_val;
        CbFsEntry entry{};
        if (fi != nullptr && fi->fh != 0) {
            entry_val = fi->fh;
            entry = state->get_entry(entry_val);
        } else {
            entry = state->get_entry(path);
            entry_val = entry.entry_id;
            std::cout << " >> write(" << path << ", " << fi << ", " << entry_val << ")" << std::endl;
        }

        const size_t want_size = size + offset;

        if (want_size > entry.size_bytes) {
            cbfs_error::check_return(cbfs_truncate(state->fs, entry_val, want_size));
        }

        uint32_t size_u32 = static_cast<uint32_t>(size);
        cbfs_error::check_return(cbfs_write_entry_data(state->fs, entry_val, offset, reinterpret_cast<const uint8_t*>(data), &size_u32));
        return size_u32;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_truncate(
    const char* path,
    fuse_off_t size,
    fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "truncate(" << path << ")\n";
    }

    try {
        cbfs_error::check_return(cbfs_truncate(state->fs, fi->fh, static_cast<uint32_t>(size)));
        return 0;
    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

static int cbfs_fuse_utimens(
    const char* path,
    const fuse_timespec* tv,
    fuse_file_info* fi
) {
    const auto state = CbFuseState::get_instance();
    std::unique_lock lk(state->lock);
    if (state->can_print_debug(path)) {
        std::cout << "utimens(" << path << ", " << tv << ", " << ((tv != nullptr) ? tv->tv_sec : 0) << ")\n";
    }

    try {
        uint16_t hdl{};
        if (fi == nullptr) {
            hdl = state->get_entry(path).entry_id;
        } else {
            hdl = fi->fh;
        }

#ifdef WIN32
        constexpr int64_t UTIME_NOW = 0;
#endif

        if (tv != nullptr && tv->tv_nsec != UTIME_NOW) {
            const CbFsTime tvi = cbfs_millis_to_time(timespec_to_millis(*tv));
            cbfs_error::check_return(cbfs_set_time(state->fs, hdl, &tvi));
        } else {
            cbfs_error::check_return(cbfs_set_time(state->fs, hdl, nullptr));
        }
        return 0;

    } catch (const cbfs_error& err) {
        return err.get_return_code();
    }
}

struct options_t {
    uint16_t sector_size{ 512 };
    uint16_t sector_count{ 32768 };
    bool file_read_only;
    bool sparse;
    bool gzip;
    bool zeroblocks;
    bool randomize;
    bool func_calls;
    const char* base_file;
    const char* debug_name;
};

enum {
    KEY_VERSION,
    KEY_HELP,
};

#define CBFS_OPTION(t, p) { t, offsetof(options_t, p), 1 }

static const struct fuse_opt cbfs_option_spec[] = {
    CBFS_OPTION("secsize=%hu", sector_size),
    CBFS_OPTION("seccount=%hu", sector_count),
    CBFS_OPTION("file=%s", base_file),
    CBFS_OPTION("mem", file_read_only),
    CBFS_OPTION("gzip", gzip),
    CBFS_OPTION("sparse", sparse),
    CBFS_OPTION("zeroblocks", zeroblocks),
    CBFS_OPTION("rand", randomize),
    CBFS_OPTION("funcs", func_calls),
    CBFS_OPTION("dname=%s", debug_name),
    FUSE_OPT_KEY("--version", KEY_VERSION),
    FUSE_OPT_KEY("-V", KEY_VERSION),
    FUSE_OPT_KEY("--help", KEY_HELP),
    FUSE_OPT_KEY("-h", KEY_HELP),
    FUSE_OPT_END,
};

#undef CBFS_OPTION

static void* cbfs_fuse_init(
    struct fuse_conn_info*,
    struct fuse_config* config
) {
    const options_t options = *static_cast<options_t*>(fuse_get_context()->private_data);

    config->use_ino = false;
    config->kernel_cache = true;

    std::unique_ptr<CbFuseState> state = std::make_unique<CbFuseState>();
    state->base_file = options.base_file;
    state->read_only = options.file_read_only != 0;
    state->print_enabled = (config->debug != 0) || options.func_calls;
    state->debug_name = options.debug_name;
    state->save_gzip = options.gzip;
    state->save_sparse = options.sparse;
    state->save_zeroed = options.zeroblocks;
    state->fs = cbfs_create("cbfs", options.sector_size, options.sector_count, state->base_file, options.randomize);

    if (config->debug) {
        std::cout << "Sector Size: " << static_cast<int>(options.sector_size) << ", SectorCount: " << options.sector_count << '\n';

        if (options.base_file != nullptr) {
            std::cout << "Base File: " << options.base_file << '\n';
        }
    }

    CbFsStats fs_stats{};
    if (cbfs_get_stats(state->fs, &fs_stats) == CbFsResult::Success) {
        state->block_size = fs_stats.block_size;
    }

    return state.release();
}

static constexpr struct fuse_operations generate_fuse_opers() {
    struct fuse_operations opers{};
    opers.init = cbfs_fuse_init;
    opers.getattr = cbfs_fuse_getattr;
    opers.open = cbfs_fuse_open;
    opers.opendir = cbfs_fuse_opendir;
    opers.read = cbfs_fuse_read;
    opers.readdir = cbfs_fuse_readdir;
    opers.create = cbfs_fuse_create;
    opers.destroy = cbfs_fuse_destroy;
    opers.rename = cbfs_fuse_rename;
    opers.mkdir = cbfs_fuse_mkdir;
    opers.rmdir = cbfs_fuse_rmdir;
    opers.unlink = cbfs_fuse_unlink;
    opers.write = cbfs_fuse_write;
    opers.mknod = cbfs_fuse_mknod;
    opers.statfs = cbfs_fuse_statfs;
    opers.fsync = cbfs_fuse_fsync;
    opers.fsyncdir = cbfs_fuse_fsync;
    opers.truncate = cbfs_fuse_truncate;
    opers.utimens = cbfs_fuse_utimens;
    return opers;
}

static const struct fuse_operations cbfs_fuse_oper = generate_fuse_opers();

static int cbfs_opt_proc(
    void*,       // data
    const char*, // arg
    int key,
    struct fuse_args* outargs
) {
    switch (key) {
    case KEY_HELP:
        fprintf(
            stdout,
            "usage: %s [options] mountpoint\n"
            "\n"
            "cbfs options:\n"
            "    -o seccount=NUM\n"
            "    -o secsize=NUM\n"
            "    -o file=PATH\n"
            "    -o gzip\n"
            "    -o sparse\n"
            "    -o zeroblocks\n"
            "    -o mem\n"
            "    -o randomize\n",
            outargs->argv[0]
        );
        fuse_opt_add_arg(outargs, "-h");
        outargs->argv[0][0] = '\0';
        fuse_main(outargs->argc, outargs->argv, &cbfs_fuse_oper, nullptr);
        fuse_opt_free_args(outargs);
        exit(0);
    case KEY_VERSION:
        fprintf(stdout, "cbfs version 0.1\n");
        fuse_opt_add_arg(outargs, "--version");
        fuse_main(outargs->argc, outargs->argv, &cbfs_fuse_oper, nullptr);
        fuse_opt_free_args(outargs);
        exit(0);
    }
    return 1;
}

int main(
    int argc,
    char* argv[]
) {
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);

    options_t options{};
    fuse_opt_parse(&args, &options, cbfs_option_spec, cbfs_opt_proc);
    const auto ret = fuse_main(args.argc, args.argv, &cbfs_fuse_oper, &options);
    fuse_opt_free_args(&args);
    return ret;
}
