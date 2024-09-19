#!/usr/bin/python3
# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
# Copyright 2021-2024 Sebastian Lederer. See the file LICENSE.md for details

import struct
import sys
from collections import namedtuple
import os

MaxPartitions = 8
SlotFree = 1
SlotReserved = 2
SlotDeleted = 4
SlotEndScan = 8
SlotFirst = 16
SlotExtent = 32
SlotReadonly = 64

PartEnabled = 1
PartBoot = 2
PartLast = 4
PartPhysical = 8
PartDefault = 16

part_fmt = ">ii32siiiiii" 
dirslot_fmt = ">ii32siiiiii"

PartSlot = namedtuple("PartSlot", "namelength maxlength name flags startBlock blocks extentSize dirSize bootBlocks")
DirSlot = namedtuple("DirSlot", "namelength maxlength name flags sizeBytes createTime modTime generation owner")

def createpart(name, flags, start_block, blocks, extent_size, dir_size, boot_blocks=0):
    b = struct.pack(part_fmt, len(name), 32, bytes(name, 'utf8'), flags, start_block, blocks,
                        extent_size, dir_size, boot_blocks)
    return b


def decodepart(data):
    return struct.unpack(part_fmt, data)


def getpartslot(img, partno):
    img.seek(partno * 64)
    fields = decodepart(img.read(64))
    name = getname(fields)
    fields = list(fields)
    fields[2] = name
    return PartSlot._make(fields)


def createdirslot(name, flags, size, create_time, mod_time, generation, owner):
    return struct.pack(dirslot_fmt, len(name), 32, bytes(name, 'utf8'), flags, size, create_time,
                        mod_time, generation, owner)


def decodedirslot(data):
    return struct.unpack(dirslot_fmt, data)


def putdirslot(img, partstart, slotno, slotdata):
    img.seek(partstart * 512 + slotno * 64)
    img.write(slotdata)


def getdirslot(img, part, slotno):
    partstart = part.startBlock
    img.seek(partstart * 512 + slotno * 64)
    fields = decodedirslot(img.read(64))
    name = getname(fields)
    fields = list(fields)
    fields[2] = name
    return DirSlot._make(fields)


def writefile(img, part, slotno, databytes):
    partstart = part.startBlock
    extent_size = part.extentSize
    pos = partstart * 512 + slotno * extent_size

    print("writefile: slot", slotno, len(databytes), "bytes at block", partstart + slotno * extent_size // 512, " pos",
          pos)
    img.seek(pos)
    img.write(databytes)
    extendfile(img, part, slotno, len(databytes))


def write_bootimage(img, startblock, data):
    pos = startblock * 512
    img.seek(pos)
    img.write(data)


def extendfile(img, part, slotno, newSize):
    dirslot = getdirslot(img, part, slotno)
    extent_size = part.extentSize
    old_extents = dirslot.sizeBytes // extent_size + 1
    new_extents = newSize // extent_size + 1
    old_last_slot = slotno + 1
    new_last_slot = slotno + new_extents - 1
    print("extendfile old_last_slot {} new_last_slot {} old_ext {} new_ext {} ".format(
        old_last_slot, new_last_slot, old_extents, new_extents), end="")

    for i in range(old_last_slot, new_last_slot + 1):
        d = getdirslot(img, part, i)
        if d.flags & SlotFree:
            print(i," ", sep="", end="")
            d = createdirslot("", SlotExtent, 0, 0, 0, 0, 0)
            putdirslot(img, part.startBlock, i, d)
        else:
            print("Cannot extend file at dirslot",i, "- wanted size:", newSize)
            listdir(img, part)
            sys.exit(3)
    print()
    if old_extents != new_extents:
        print("file at dirslot",slotno, "extended to", new_extents, "extents")

    firstslot = createdirslot(dirslot.name, dirslot.flags, newSize, dirslot.createTime, dirslot.modTime,
                                dirslot.generation, dirslot.owner)
    putdirslot(img, part.startBlock, slotno, firstslot)


def findslots(img, part, size):
    size_in_extents = (size + part.extentSize - 1) // part.extentSize
    last_slot = part.dirSize - 1

    found = False
    firstslot = 0

    slotno = 0

    while slotno <= last_slot and not found:
        dirslot = getdirslot(img, part, slotno)
        if dirslot.flags & SlotFree:
            found = True
            firstslot = slotno
            print("found free slot at",slotno, ":", dirslot)
            if size_in_extents > 1:
                print("checking slot ", end='')
                for s in range(1, size_in_extents):
                    slotno += 1
                    print(slotno, " ", end='')
                    dirslot = getdirslot(img, part, slotno)
                    if not (dirslot.flags & SlotFree):
                        print("wanted slot",slotno,"not free")
                        found = False
                        break
                print()
        else:
            slotno += 1

    if found:
        return firstslot
    else:
        return 0


def putfile(infilename, filename, img, part, partstart, slotnr):
    if filename is None:
        filename = os.path.basename(infilename)
    try:
        extent_size = part.extentSize

        with open(infilename,"rb") as infile:
            print("creating file", filename, "at slot", slotnr)
            content = infile.read()
            d = createdirslot(filename, SlotFirst, len(content), 0, 0, 0, 0)
            putdirslot(img, partstart, slotnr, d)
            writefile(img, part, slotnr, content)
            slotnr += len(content) // extent_size + 1
    except Exception as e:
        print("error reading file", infilename, "skipping", e)
    return slotnr


def getname(data):
    length = data[0]
    name = data[2][:length].decode('utf8')
    return name


def flags2str(flags):
    result = ""
    if flags & SlotFree:
        result += "F"
    if flags & SlotReserved:
        result += "R"
    if flags & SlotDeleted:
        result += "D"
    if flags & SlotEndScan:
        result += "E"
    if flags & SlotFirst:
        result += "1"
    if flags & SlotExtent:
        result += "+"
    if flags & SlotReadonly:
        result += "o"
    return result


def findvolume(img, volname):
    part = None
    partno = 0

    while True:
        part = getpartslot(img, partno)
        if part.flags & PartEnabled:
            if part.name == volname:
                break

        partno += 1
        if (part.flags & PartLast) or partno >= MaxPartitions:
            part = None
            break

    return part


def listvolumes(img):
    firstvolume = None
    partno = 0
    done = False
    while not done:
        part = getpartslot(img, partno)
        print(part)

        if part.flags & PartEnabled:
            print("part", partno, " enabled")
            print("\tvolume name\t", part.name)
            print("\tstart block\t", part.startBlock)
            print("\tblocks\t\t", part.blocks)
            print("\textentSize\t", part.extentSize)
            print("\tdirSize\t\t", part.dirSize)
            if firstvolume is None :
                firstvolume = part

        partno += 1
        if (part.flags & PartLast) or partno >= MaxPartitions:
            done = True

    return firstvolume

 
def listdir(img, part, verbose=False):
    print("Directory of {}:".format(part.name))
    slotno = 0
    done = False
    while not done:
        slot = getdirslot(img, part, slotno)
        if (slot.flags & SlotFirst):
            print(slot.name, slot.sizeBytes, slotno)
        else:
            if verbose:
                print(flags2str(slot.flags))
        slotno += 1
        #if (slot.flags & SlotEndScan) or (slotno >= part.dirSize):
        #    done = True
        if (slotno >= part.dirSize):
            done = True
	

def findfile(img, part, name):
    slotno = 0
    done = False
    while not done:
        slot = getdirslot(img, part, slotno)
        if (slot.flags & SlotFirst) and not (slot.flags & SlotDeleted):
            if slot.name == name:
                return slotno
        slotno += 1
        if (slot.flags & SlotEndScan) or (slotno >= part.dirSize):
            done = True

    return None


def readfile(img, part, slotno):
    pos = part.startBlock * 512 + slotno * part.extentSize
    dirslot = getdirslot(img, part, slotno)
    size = dirslot.sizeBytes

    print("readfile", dirslot.name, size,"bytes from",pos)

    img.seek(pos)
    return img.read(size)


def parsepath(img, pathname):
    volname = "SYSTEM"
    if pathname.startswith("#"):
        volname, filename = pathname.split(':')
        volname = volname[1:]
        vol = findvolume(img, volname)
        if vol is None:
            print("Volume {} not found".format(volname))
            return (None, None)
    else:
        filename = pathname
        vol = listvolumes(img)

    return (vol, filename)


def readfromimg(img, pathname,outfilepath):
    vol, filename = parsepath(img, pathname)
    if vol is None:
        return

    listdir(img, vol)

    slotno = findfile(img, vol, filename)
    if slotno is None:
        print("File", filename,"not found")
        return

    data = readfile(img, vol, slotno)

    with open(outfilepath, "wb") as f:
        f.write(data)


def writetoimg(img, pathname, infilepath):
    vol, filename = parsepath(img, pathname)
    if vol is None:
        return

    existing_slot = findfile(img, vol, filename)
    if existing_slot is not None:
        print("Filename", filename, "already exists on", vol.name)
        return

    filesize = os.path.getsize(infilepath)

    slotno = findslots(img, vol, filesize)
    if slotno < 1:
        print("No space on volume", vol.name)
        return

    putfile(infilepath, filename, img, vol, vol.startBlock, slotno)


def create_image_with_stuff():
    imgfile = "sdcard.img"
    bootimage = "../lib/coreloader.prog"
    dir_slots = 256
    extent_size = 8192
    slots_per_extent = extent_size // 64
    reserved_slots = dir_slots // slots_per_extent

    f = open(imgfile,"w+b")

    b = createpart("PHYS", PartPhysical, 0, 12288, 4096, 0, 0)
    #print(b)
    f.write(b)

    with open(bootimage, "rb") as bf:
        bootdata = bf.read()
    bootBlocks = len(bootdata) // 512 + 1
    b = createpart("BOOT", PartBoot, 16, 112, 0, 0, bootBlocks)
    f.write(b)
    b = createpart("Testvolume 1", PartEnabled, 128, 3968, 8192, 248)
    f.write(b)

    b = createpart("SYSTEM", PartEnabled, 4096, 4096, 8192, 256)
    f.write(b)

    b = createpart("Examples", PartEnabled + PartLast, 8192, 4096, 8192, 256)
    f.write(b)

    part = getpartslot(f, 2)
    partstart = part.startBlock
    dir_slots = part.dirSize

    print("creating",reserved_slots, "reserved directory slots")
    for a in range(0,reserved_slots):
        d = createdirslot("DIR", SlotReserved, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, a, d)

    print("creating", dir_slots - reserved_slots, "free slots")

    for a in range(reserved_slots,  dir_slots):
        d = createdirslot("", SlotFree, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, a, d)

    #d = createdirslot("obstacle", SlotFirst , 0, 0, 0, 0, 0)
    #putdirslot(f, partstart, reserved_slots + 2, d)


    slotnr = reserved_slots

    if True:
        data = bytes("ABCDEFGHIJKLMNOPQRST", "ASCII") * 410 + bytes('1234','ASCII')
        d = createdirslot("A Testfile.text", SlotFirst, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, slotnr, d)
        writefile(f, part, slotnr, data)
        slotnr += len(data) // extent_size + 1


        d = createdirslot("", SlotFree, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, slotnr, d)
        slotnr += 1

        d = createdirslot("Another_file.text", SlotFirst, 20, 0, 0, 0, 0)
        putdirslot(f, partstart, slotnr, d)
        slotnr += 1

    for a in range(0,20):
        d = createdirslot("", SlotFree, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, slotnr, d)
        slotnr += 1

    if True:
        d = createdirslot("test3.text", SlotFirst, 20, 0, 0, 0, 0)
        putdirslot(f, partstart, slotnr, d)
        slotnr += 1

        d = createdirslot("test1.text", SlotFirst, 20, 0, 0, 0, 0)
        putdirslot(f, partstart, slotnr, d)
        slotnr += 1

    d = createdirslot("test2.text", SlotFirst, 20, 0, 0, 0, 0)
    putdirslot(f, partstart, slotnr, d)
    slotnr += 1

    slotnr = putfile("../examples/test.txt", "sometext.text" , f, part, partstart, slotnr)

    slotnr = putfile("../examples/chase.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../examples/chase.pas", None , f, part, partstart, slotnr)
    slotnr = putfile("../examples/sine.pas", None , f, part, partstart, slotnr)
    slotnr = putfile("../examples/graph2.pas", None , f, part, partstart, slotnr)

    while slotnr < dir_slots:
        d = createdirslot("", SlotFree + SlotEndScan , 0, 0, 0, 0, 0)
        putdirslot(f, partstart, slotnr, d)
        slotnr += 1


    # second partition (SYSTEM)
    part = getpartslot(f, 3)
    partstart = part.startBlock
    dir_slots = part.dirSize

    print()
    print("Partition {} at {}".format(part.name, part.startBlock))
    print("creating",reserved_slots, "reserved directory slots")
    for a in range(0,reserved_slots):
        d = createdirslot("DIR", SlotReserved, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, a, d)

    print("creating", dir_slots - reserved_slots, "free slots")

    for a in range(reserved_slots,  dir_slots):
        d = createdirslot("", SlotFree + SlotEndScan, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, a, d)

    slotnr = reserved_slots

    slotnr = putfile("../progs/shell.prog", "shell.prog", f, part, partstart, slotnr)
    slotnr = putfile("../lib/coreloader.lsym", "coreloader.lsym", f, part, partstart, slotnr)
    slotnr = putfile("../lib/coreloader.prog", "coreloader.prog", f, part, partstart, slotnr)

    slotnr = putfile("../lib/float32.lib", "float32.lib", f, part, partstart, slotnr)
    slotnr = putfile("../lib/runtime.lib", "runtime.lib", f, part, partstart, slotnr)
    slotnr = putfile("../lib/stdlib.lib", None, f, part, partstart, slotnr)
    slotnr = putfile("../lib/stdlib.inc", None, f, part, partstart, slotnr)

    slotnr = putfile("../pcomp/sasm.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../pcomp/pcomp.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../pcomp/lsymgen.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../pcomp/libgen.prog", None , f, part, partstart, slotnr)

    slotnr = putfile("../progs/reclaim.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../progs/dumpdir.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../progs/partmgr.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../progs/editor.pas", None , f, part, partstart, slotnr)
    slotnr = putfile("../progs/editor.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../progs/xfer.prog", None , f, part, partstart, slotnr)

    listdir(f, part)

    # third partition
    part = getpartslot(f, 4)
    partstart = part.startBlock
    dir_slots = part.dirSize

    print()
    print("Partition {} at {}".format(part.name, part.startBlock))

    print("creating",reserved_slots, "reserved directory slots")
    for a in range(0,reserved_slots):
        d = createdirslot("DIR", SlotReserved, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, a, d)

    slotnr = reserved_slots

    print("creating", dir_slots - reserved_slots, "free slots")

    for a in range(reserved_slots,  dir_slots):
        d = createdirslot("", SlotFree + SlotEndScan, 0, 0, 0, 0, 0)
        putdirslot(f, partstart, a, d)

    slotnr = putfile("../examples/helloasm.s", None, f, part, partstart, slotnr)
    # slotnr = putfile("helloasm.prog", "helloasm.prog", f, part, partstart, slotnr)

    # slotnr = putfile("hello.prog", None , f, part, partstart, slotnr)
    # slotnr = putfile("hellop.s", None , f, part, partstart, slotnr)
    slotnr = putfile("../examples/hellop.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../tests/timetest.pas", None , f, part, partstart, slotnr)
    # slotnr = putfile("../tests/timetest.prog", None , f, part, partstart, slotnr)

    slotnr = putfile("../tests/readtest.pas", None , f, part, partstart, slotnr)
    slotnr = putfile("../tests/readtest.prog", None , f, part, partstart, slotnr)

    slotnr = putfile("../tests/readchartest.pas", None , f, part, partstart, slotnr)
    slotnr = putfile("../tests/readchartest.prog", None , f, part, partstart, slotnr)

    # slotnr = putfile("cchangetest.pas", None , f, part, partstart, slotnr)
    # slotnr = putfile("cchangetest.prog", None , f, part, partstart, slotnr)

    slotnr = putfile("../tests/test109.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../tests/test133.pas", None , f, part, partstart, slotnr)
    # slotnr = putfile("../tests/test133.prog", None , f, part, partstart, slotnr)

    slotnr = putfile("../tests/test159.pas", None , f, part, partstart, slotnr)
    # slotnr = putfile("../tests/test159.prog", None , f, part, partstart, slotnr)

    slotnr = putfile("../tests/umlaut.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../examples/rtpair.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../examples/5cubes.pas", None , f, part, partstart, slotnr)
    # slotnr = putfile("../examples/5cubes.prog", None , f, part, partstart, slotnr)

    slotnr = putfile("../examples/3dcube.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../examples/conway.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../examples/mandelbrot.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../examples/lines.pas", None , f, part, partstart, slotnr)

    slotnr = putfile("../examples/viewpict.pas", None , f, part, partstart, slotnr)
    # slotnr = putfile("viewpict.prog", None , f, part, partstart, slotnr)
    slotnr = putfile("../examples/ara.pict", "ara.pict" , f, part, partstart, slotnr)
    slotnr = putfile("../examples/shinkansen.pict", "shinkansen.pict" , f, part, partstart, slotnr)
    slotnr = putfile("../examples/snow_leopard.pict", "snow_leopard.pict" , f, part, partstart, slotnr)

    listdir(f, part)

    write_bootimage(f, 16, bootdata)

    f.close()


if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1] == "get":
            f = open(sys.argv[2], "rb")
            readfromimg(f, sys.argv[3], sys.argv[4])
        elif sys.argv[1] == "put":
            imgfile = open(sys.argv[2], "r+b")
            infilepath = sys.argv[3]
            destfilename = sys.argv[4]
            writetoimg(imgfile, destfilename, infilepath)
        elif sys.argv[1] == "createimg":
            create_image_with_stuff()
        sys.exit(0)
