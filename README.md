# android-hs-mobile-common

A native code library contains any common utils for android ndk develop.

It was wroted by haskell according [haskell-android-ndk-cross-compiler](https://github.com/Guang1234567/ghc-cross-compiler-for-android/tree/v2-ghc-8.6.5-20190531-aarch64-linux-android)


## Feature

- The haskell wrapper of android logger.
- Some `FFI` demo.
    - How C++ `char **` or `char*[]` interface with haskell `Ptr CString`.
    - Some `FFI` utils function to simplify the manipulating of `C String`.
    - ...
- ...


## How to test?

See [demo](https://github.com/Guang1234567/hs-android-helloworld/blob/3de24cc386b6771756330a08c6b9d779492a59df/app/CMakeLists.txt#L70-L72).