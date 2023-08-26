# Antenna Rotator "Rotor" Control Software (Rotor-EZ protocol)
A simple, efficient way to remote-control antenna rotator control boxes running the Rotor-EZ protocol.

![The Antenna Rotator Control Software](./help/Main_Window_Large.png "The Antenna Rotator Control Software")

By Bertrand Zauhar, VE2ZAZ / VA2IW   
https://ve2zaz.net   
https://github.com/VE2ZAZ   
https://www.qrz.com/db/VE2ZAZ   

### Overview
This Antenna Rotator Control Software offers a simple, practical way of remote operating antenna rotator (aka “rotor”) control boxes compatible with the Rotor-EZ protocol, in particular the VE2ZAZ Smart Rotator Controller, which was designed as an add-on to HyGain Tailtwister, Ham-III and Ham-IV controller boxes. The software main features are:
- It offers a PC interface to the VE2ZAZ Smart Rotator Controller,
- It can also control any antenna rotator control box compatible with the Rotor-EZ protocol. It may also work with DCU-1 protocol boxes, since DCU-1 is a subset of the Rotor-EZ protocol. Presumably, the DCU-1 box will reject the un-recongnized Rotor-EZ commands, such as reading the current heading.
- It offers a re-sizeable window to accommodate various screen needs,
- It displays the current heading on a station-centered azimuthal map,
- The azimuthal map is mouse-clickable to quickly pick a target heading,
- The Azimuthal map can be customized for the station location,
- The window offers 8 memory buttons for frequently used headings,
- It has a Stop button to interrupt the ongoing rotation,
- The program can receive N1MM+ logger “Rotate” and “Stop Rotate” commands via UDP socket,
- User settings such as window size and location, serial port and azimuthal map file name are saved at program exit, and restored at program launch. The settings file resides in the same directory as the program directory. 
- Software executables are available for Ubuntu-Linux and Windows 10. The software may be compiled for other OS releases, such as MacOS with the Lazarus IDE, however it has not been tested by the author.

For more details on operation, configuration and licensing of this software, please refer to the [help.pdf](./help/help.pdf) file.
