#include <assert.h>
#include <cerrno>
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

#include <iostream>

/*
 * Command line options
 *
 * We can't set default values for the char* fields here because
 * fuse_opt_parse would attempt to free() them when the user specifies
 * different values on the command line.
 */
struct options_t {
  uint16_t sector_size;
  uint16_t sector_count;
  const char* base_file;
  int read_only;
  int show_help;
};

#define OPTION(t, p) {t, offsetof(options_t, p), 1}

static const struct fuse_opt option_spec[] = {
    OPTION("--sector-size=%hu", sector_size),
    OPTION("-s=%hu", sector_size),
    OPTION("--sector-count=%hu", sector_count),
    OPTION("-S=%hu", sector_count),
    OPTION("--base-file=%s", base_file),
    OPTION("-b=%s", base_file),
    OPTION("-r", read_only),
    OPTION("--read-only", read_only),
    OPTION("-h", show_help),
    OPTION("--help", show_help),
    FUSE_OPT_END};

struct CbFuseState {
  CbFileSystem* fs;
  const char* base_file;
  bool read_only;
};

static void cbfs_fuse_destroy(void* state) {
  if (state != nullptr) {
    auto ctx = static_cast<CbFuseState*>(state);

    if (ctx->base_file != nullptr && !ctx->read_only) {
      cbfs_save(ctx->fs, ctx->base_file);
    }

    cbfs_destroy(ctx->fs);

    delete ctx;
  }
}

static int cbfs_fuse_getattr(const char* path,
                             struct stat* stbuf,
                             [[maybe_unused]] struct fuse_file_info* fi) {
  const auto ctx = fuse_get_context();
  const auto state = static_cast<CbFuseState*>(ctx->private_data);

  CbCompatFsStats stats{};
  if (const auto res = cbfs_get_stats(state->fs, &stats); res != 0) {
    return -ENOSYS;
  }

  CbCompatEntry entry{};
  if (const auto res = cbfs_get_entry(state->fs, path, &entry); res != 0) {
    return -ENOENT;
  }

  *stbuf = {};
  stbuf->st_ino = entry.entry_id;
  stbuf->st_blksize = stats.block_size;
  stbuf->st_blocks =
      entry.size_bytes / 512 + ((entry.size_bytes == 512) ? 0 : 1);
  stbuf->st_uid = ctx->uid;
  stbuf->st_gid = ctx->gid;
  stbuf->st_mode =
      ((entry.entry_type == CbEntryType::Directory) ? S_IFDIR : S_IFREG) | 0755;
  stbuf->st_size = entry.size_bytes;

  return 0;
}

static int cbfs_fuse_readdir(const char* path,
                             void* buf,
                             fuse_fill_dir_t filler,
                             off_t offset,
                             [[maybe_unused]] struct fuse_file_info* fi,
                             [[maybe_unused]] fuse_readdir_flags flags) {

  const auto state =
      static_cast<CbFuseState*>(fuse_get_context()->private_data);

  std::cout << "Reading " << path << '\n';

  CbCompatEntry entry{};
  if (cbfs_get_entry(state->fs, path, &entry) != 0 ||
      entry.entry_type != CbEntryType::Directory) {
    std::cout << "Error in " << path << '\n';
    return -ENOENT;
  }

  const auto entries = cbfs_read_dir(state->fs, entry.entry_id);

  std::cout << "Found " << entries.size << " entries" << std::endl;

  if (entries.entries != nullptr) {
    for (size_t i = offset; i < entries.size + 2; ++i) {
      if (i == 0) {
        if (0 != filler(buf, ".", NULL, 0, FUSE_FILL_DIR_PLUS)) {
          return -ENOSYS;
        }
      } else if (i == 1) {
        if (0 != filler(buf, "..", NULL, 0, FUSE_FILL_DIR_PLUS)) {
          return -ENOSYS;
        }
      } else {
        if (0 != filler(buf,
                        entries.entries[i - 2].name,
                        NULL,
                        0,
                        FUSE_FILL_DIR_PLUS)) {
          return -ENOSYS;
        }
      }
    }
  } else {
    return -ENOENT;
  }

  return 0;
}

static int cbfs_fuse_open(const char* path, struct fuse_file_info* fi) {
  const auto state =
      static_cast<CbFuseState*>(fuse_get_context()->private_data);

  CbCompatEntry entry{};
  if (0 != cbfs_get_entry(state->fs, path, &entry)) {
    return -ENOENT;
  }

  if ((fi->flags & O_ACCMODE) != O_RDONLY) {
    return -EACCES;
  }

  return 0;
}

static int cbfs_fuse_read(const char* path,
                          char* buf,
                          size_t size,
                          off_t offset,
                          [[maybe_unused]] struct fuse_file_info* fi) {
  const auto state =
      static_cast<CbFuseState*>(fuse_get_context()->private_data);

  std::cout << "Attempting to read " << path << " with size " << size
            << std::endl;

  CbCompatEntry entry{};
  if (0 != cbfs_get_entry(state->fs, path, &entry)) {
    std::cout << "NO entry\n";
    return -ENOENT;
  }

  if (size > std::numeric_limits<int32_t>::max()) {
    return -ENOSYS;
  }

  uint32_t size_u32 = size;

  if (0 != cbfs_read_entry_data(state->fs,
                                entry.entry_id,
                                offset,
                                reinterpret_cast<uint8_t*>(buf),
                                &size_u32)) {
    std::cout << "NO data\n";
    return -ENOSYS;
  }

  return size_u32;
}

int cbfs_fuse_mkdir(const char* path, mode_t mode) {
  const auto state =
      static_cast<CbFuseState*>(fuse_get_context()->private_data);

    std::cout << "mkdir " << path << std::endl;

  (void)mode;
  if (0 == cbfs_mkdir(state->fs, path)) {
      return 0;
  } else {
      return -ENOENT;
  }
}

int cbfs_fuse_unlink(const char* path) {
    std::cout << "Unlinking " << path << std::endl;
  const auto state =
      static_cast<CbFuseState*>(fuse_get_context()->private_data);

  if (0 != cbfs_remove_entry(state->fs, path, CbEntryType::File)) {
    return -ENOENT;
  } else {
    return 0;
  }
}

int cbfs_fuse_rmdir(const char* path) {
    std::cout << "rmdir " << path << std::endl;

  const auto state =
      static_cast<CbFuseState*>(fuse_get_context()->private_data);

  if (0 != cbfs_remove_entry(state->fs, path, CbEntryType::Directory)) {
    return -ENOENT;
  } else {
    return 0;
  }
}

static constexpr struct fuse_operations generate_fuse_opers() {
  struct fuse_operations opers{};
  opers.getattr = cbfs_fuse_getattr;
  opers.open = cbfs_fuse_open;
  opers.read = cbfs_fuse_read;
  opers.readdir = cbfs_fuse_readdir;
  opers.destroy = cbfs_fuse_destroy;
  // opers.rename = cbfs_fuse_rename;
  opers.mkdir = cbfs_fuse_mkdir;
  opers.rmdir = cbfs_fuse_rmdir;
  opers.unlink = cbfs_fuse_unlink;
  // opers.write = cbfs_fuse_write;
  // opers.create = cbfs_fuse_create;
  return opers;
}

static const struct fuse_operations cbfs_fuse_oper = generate_fuse_opers();

static void show_help(const char* progname) {
  printf("usage: %s [options] <mountpoint>\n\n", progname);
  printf(
      "File-system specific options:\n"
      "    --sector-size=<hu>, -s=<hu>   Sector Size\n"
      "    --sector-count=<hu>, -S=<hu>  Sector Count\n"
      "    --base-file=<s>, -b=<s>       Base File\n"
      "    --read-only, -r               Read Only (only reads the base file)\n"
      "\n");
}

int main(int argc, char* argv[]) {
  struct fuse_args args = FUSE_ARGS_INIT(argc, argv);

  options_t options{};
  options.base_file = nullptr;
  options.sector_count = 32768;
  options.sector_size = 512;
  options.read_only = 0;

  // Parse options
  if (fuse_opt_parse(&args, &options, option_spec, NULL) == -1) {
    return 1;
  }

  printf("Sector Size: %hu, SectorCount: %hu\n",
         options.sector_size,
         options.sector_count);

  if (options.base_file != nullptr) {
    printf("Base File: %s\n", options.base_file);
  }

  // When --help is specified, first print our own file-system
  // specific help text, then signal fuse_main to show
  // additional help (by adding `--help` to the options again)
  // without usage: line (by setting argv[0] to the empty
  // string)
  if (options.show_help) {
    show_help(argv[0]);
    assert(fuse_opt_add_arg(&args, "--help") == 0);
    args.argv[0][0] = '\0';
  }

  std::unique_ptr<CbFuseState> state = std::make_unique<CbFuseState>();
  state->fs = cbfs_create(
      "cbfs", options.sector_size, options.sector_count, options.base_file);
  state->base_file = options.base_file;
  state->read_only = options.read_only != 0;

  const auto ret =
      fuse_main(args.argc, args.argv, &cbfs_fuse_oper, state.release());
  fuse_opt_free_args(&args);
  return ret;
}
