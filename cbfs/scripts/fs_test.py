import os
from pathlib import Path
import time
import signal
import argparse


def main():
    prog = argparse.ArgumentParser(description="Simple filesystem tester")
    prog.add_argument("fs_base", help="the base folder of the filesystme to test")

    args = prog.parse_args()

    base_folder = Path(args.fs_base)

    vs = os.statvfs(base_folder)
    init_nodes = vs.f_bfree

    MAX_ITER = 1000

    should_continue = True
    def stop_handler(*_):
        nonlocal should_continue
        should_continue = False

    signal.signal(signalnum=signal.SIGINT, handler=stop_handler)
    signal.signal(signalnum=signal.SIGTERM, handler=stop_handler)

    for i in range(MAX_ITER):
        if not should_continue:
            break

        t1 = time.time()
        F1_SIZE = 1000
        files: list[Path] = []

        print(f"{i + 1} / {MAX_ITER}")

        for j in range(258):
            f = base_folder / f"file{j}.bin"
            f.write_bytes(bytes([(i + j + ii) % 256 for ii in range(F1_SIZE)]))

            fs = os.stat(f)
            assert fs.st_size == F1_SIZE

            vs = os.statvfs(base_folder)
            assert init_nodes > vs.f_bfree

            files.append(f)

        for k in range(10):
            folder = base_folder / f"folder{k}"
            folder.mkdir()

            for j in range(264):
                f = folder / f"abc_{j}.bin"
                f.write_bytes(bytes([(i + ii + j + k) % 256 for ii in range(F1_SIZE)]))

                fs = os.stat(f)
                assert fs.st_size == F1_SIZE

                vs = os.statvfs(base_folder)
                assert init_nodes > vs.f_bfree

            files.append(folder)

        for k in range(10):
            folder = base_folder / f"folder_2_{k}"
            folder.mkdir()

            in_files: list[Path] = []

            for j in range(264):
                f = folder / f"abcd_{j}.bin"
                f.write_bytes(bytes([(i + ii + j + k) % 256 for ii in range(F1_SIZE)]))

                fs = os.stat(f)
                assert fs.st_size == F1_SIZE

                vs = os.statvfs(base_folder)
                assert init_nodes > vs.f_bfree
                in_files.append(f)

            for f in in_files:
                f.rename(folder / "file.bin")

            files.append(folder)

        for f in files:
            if f.is_dir():
                f.rmdir()
            else:
                f.unlink()

        vs = os.statvfs(base_folder)
        print(f"  {init_nodes} - {vs.f_bfree} = {init_nodes - vs.f_bfree}")
        assert init_nodes == vs.f_bfree
        t2 =time.time()
        print(f"  {t2 - t1:.2f} s")

    vs = os.statvfs(base_folder)
    assert init_nodes == vs.f_bfree


if __name__ == "__main__":
    main()
