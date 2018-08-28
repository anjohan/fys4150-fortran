cmake_minimum_required(VERSION 3.9)
project(1)
enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fcheck=all")
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -march=native")
endif()

add_executable(example ${CMAKE_CURRENT_SOURCE_DIR}/example.f90)

foreach(version general special)
    add_library(${version}_lib ${CMAKE_CURRENT_SOURCE_DIR}/${version}.f90)
    target_link_libraries(example ${version}_lib)
endforeach(version)


