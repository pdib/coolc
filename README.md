# Cool Compiler project

Builds with the following deps (you may be able to build with older deps, but at least these should work):

- Bison 3.0
- Flex 2.5
- G++ 6.2

If you're building in Windows Subsytem for Linux, everything should be alright, except you may have to install G++ 6.x manually.
Once you've done that, the compiled binary may complain about `version CXXABI_1.3.9 not found`.
This means you have to update your `LD_LIBRARY_PATH` env variable to have the folder that contains `libstdc++.so`.

Something like 
```
export LD_LIBRARY_PATH=/usr/local/lib64/:$LD_LIBRARY_PATH
```