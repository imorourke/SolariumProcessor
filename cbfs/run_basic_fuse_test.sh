#!/usr/bin/bash

set -e

START="$(pwd)"
TEST_DIR="$START/mnt"
TEST_FILE="$START/test.cbfs"
mounted=0
success=0

mkdir -p "$TEST_DIR"

function on_error()
{
    set +e
    if [[ $mounted != 0 ]]; then
        sleep 1
        umount "$TEST_DIR"
    fi
    rm -r "$TEST_DIR"
    rm -f "$TEST_FILE"

    if [[ $success == 0 ]]; then
        exit 1
    fi
}

trap on_error SIGINT
trap on_error EXIT

cargo run -- "$TEST_DIR" -b "$TEST_FILE" &
RESULT_PID=$!
mounted=1

sleep 1

cd "$TEST_DIR"
touch a.txt
echo "This is a test!" > b.txt
mkdir c
cd c
echo "Hello, world!" > c.txt
cp c.txt ../d.txt
mkdir f1
mv ../d.txt f1/e.txt
mv f1 ../

cd "$START"
umount "$TEST_DIR"
mounted=0

wait $RESULT_PID
RESULT=$?

if [[ $RESULT != 0 ]]; then
    echo "Invalid result for background process"
    exit 1
fi

success=1
