Semester-2016-2
===============



Repository for the INE - UFSC Operating Systems' Graduation subject.


# Installation

1. To install EPOS 2.0 for the "EPOSMote III" device, visit the LISHA official Website:
  * [Download Page](https://epos.lisha.ufsc.br/EPOS+Software)
  * GCC 7.2.0 - Intel x86 (IA32)
  * EPOS 2.0
    * OBS:
      You need to register/create a free account,
      in order to be able to see the EPOS 1.1 or 2.0 manuals.
    * See also the [EPOS 1.1 User's Guide](http://epos.lisha.ufsc.br/EPOS+User+Guide).

1. To extract the contents of `EPOS 2.0` file where you would like most.
1. To clone this repository where you would like most.
1. To extract the contents of "GCC 7.2.0 - Intel x86 (IA32)" file at the directory:
  * /usr/local/ia32/
    * OBS:
      You may need to rename the folder "gcc-4.4.4",
      inside "arm-gcc-4.4.4.tar" to "gcc".
  * For instance,
    if you downloaded the arm toolchain,
    you should extract it at /usr/local/amr/
    * If you do not have access to that path,
      you'll have to adjust the makedefs file in EPOS' main directory accordingly.
1. To place these environment variables bellow  to your user's bash configuration file "\~/.bashrc").
   To open the "\~/.bashrc",
   just use a command as gedit \~/.bashrc
```
# Replace this '/place/here/the/path/to/epos/path' by the path to folder "OpenEPOS-1.1RC"
# where you extracted the contents of the "OpenEPOS-1.1RC.tgz" file on the step 1.
export EPOS=/place/here/the/path/to/epos/path

# This configures the system to compile the EPOS applications
export PATH=/usr/local/arm/gcc/bin:$EPOS/bin:$PATH
```
1. Run these commands:
```
sudo apt-get install bin86
```



## Creating an EPOS Application

Create a new file in $EPOS/app:
* $EPOS/app/helloworld.cc

```cpp
#include <utility/ostream.h>

__USING_SYS

OStream cout;

int main() {
    cout << "\n Hello World! \n";
    return 0;
}
```



## Compiling EPOS

At the directory where you installed EPOS' source code, just type:
* $ make

The system will be configured and compiled (i.e. generated) successive times for each
application found in the app directory. Both software and hardware components will be
generated according with each application needs and stored in the img directory.



## Building an EPOS Application
If you have multiple applications or multiple deployment scenarios, but want to operate
on a single one, you can specify it using the APPLICATION parameter like this:
* $ make APPLICATION=helloworld
  * OBS: You must to compile EPOS with "make" as explained above,
before to compile one single APPLICATION.

If everything goes right, you should end with something like this:
```cpp
EPOS bootable image tool

  EPOS mode: library
  Machine: cortex_m
  Model: emote3
  Processor: armv7 (32 bits, little-endian)
  Memory: 31 KBytes
  Boot Length: 0 - 0 (min - max) KBytes
  Node id: will get from the network

  Creating EPOS bootable image in "hello.img":
    Adding application "hello": done.

  Adding specific boot features of "cortex_m": done.

  Image successfully generated (52686 bytes)!
```



## Running

First of all, you'll need to install a platform specific back-end for
EPOS to run on. During development, this is usually a
[QEMU virtual machine](http://www.qemu.org/) for your target architecture
(e.g. qemu-system-i386, qemu-system-arm). Then, simply type:
* $ make [APPLICATION=<application>] run









