# ringo
 [![Build Status](https://travis-ci.org/abhin4v/ringo.svg?branch=master)](https://travis-ci.org/abhin4v/ringo)

 A tool to transform Postgres OLTP database schemas to OLAP database schemas automatically.
 
## Setup instructions
- Install from source:
 - Install stack from http://haskellstack.org.
 - Git clone this repository:
 
   ```sh
   > git clone --recursive https://github.com/abhin4v/ringo
   ```
 - Run the following commands in the terminal:
 
   ```sh
   > cd ringo
   > stack setup
   > stack install
   ```
 - Add `~/.local/bin/` to your shell's PATH.
 - You should be able to run the command `ringo` on terminal now. Run `ringo -h` to see the help.
