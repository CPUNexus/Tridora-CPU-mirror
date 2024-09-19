#!/usr/bin/python3
# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
# Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details 

import sys
import subprocess
import os

suffixes = [ '.teeny', '.pas' ]
compiler = 'pcomp'
#assembler = '..\sasm\sasm.py'
assembler = 'sasm'
emulator = 's4emu.py'

asm_include_path = '../lib'

def run_compiler(filename, opts):
    print("compiling {}...".format(filename))
    args = [compiler]
    args.extend(opts)
    args.append(filename)
    #print("args:",args)
    status = subprocess.call(args)
    if status != 0:
        sys.exit(2)


def run_assembler(filename):
    print("assembling {}...".format(filename))
    args = [assembler]
    # args.extend([ '-I', asm_include_path])
    args.append(filename)
    status = subprocess.call(args)
    if status != 0:
        sys.exit(3)


def run_emulator(filename, extra_args):
    args = ['py', emulator, '-a', '24576', filename ]
    args.extend(extra_args)
    status = subprocess.call(args)
    if status != 0:
        sys.exit(4)


def get_compiler_options():
    comp_options = [ "-n", "-s", "-e", "-R", "-S", "-H" ]
    result = []
    while len(sys.argv) > 1 and sys.argv[1] in comp_options:
        result.append(sys.argv[1])
       	if sys.argv[1] == "-H":
       	    sys.argv.pop(1)
            result.append(sys.argv[1])
        sys.argv.pop(1)
    # print("Compiler options:",result, sys.argv[1])
    return result


def main():
    do_compile = True
    do_assemble = True
    do_emulator = False
    if len(sys.argv) < 2:
        print("Usage: {} <input file>".format(sys.argv[0]))
        sys.exit(1)

    compiler_options = get_compiler_options()
    infilename = sys.argv[1]
    basename = infilename

    if infilename.endswith('.s'):
        do_compile = False
        basename = infilename[:-2]
    elif infilename.endswith('.bin') or infilename.endswith('.prog'):
        do_compile = False
        do_assemble = False
        do_emulator = True
        basename = infilename[:-4]
    else:
        fname, suffix = os.path.splitext(infilename)
        if suffix in suffixes:
            print("#############",fname, "####",suffix)
            basename = fname

    asmfilename = basename + '.s'
    #binfilename = basename + '.bin'
    binfilename = basename + '.prog'

    if "-n" in compiler_options:
        # Assembling stdlib won't work
        do_assemble = False
        do_emulator = False

    if do_compile:
        run_compiler(infilename, compiler_options)
    if do_assemble:
        run_assembler(asmfilename)
    if do_emulator:
        run_emulator(binfilename, sys.argv[2:])

if __name__ == '__main__':
    main()
