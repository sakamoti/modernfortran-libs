cmake_minimum_required (VERSION 3.10.2)
project (modernfortran-libs 
  VERSION 0.1.0
  DESCRIPTION  "Modern fortran library"
  LANGUAGES Fortran
  )
set(CMAKE_VERBOSE_MAKEFILE OFF)
set(CMAKE_BUILD_TYPE Debug)
#set(CMAKE_BUILD_TYPE Release)
option(BUILD_SHARED_LIBS "Shared library ON/OFF" ON)
enable_testing()

#------------
# Set fortran compiler flag

#------------
# Check BLAS and LAPACK 
#find_package(BLAS REQUIRED)
#find_package(LAPACK REQUIRED)
#------------
# Check OpenMP and if it found, add OpenMPflag
option(ENABLE_OpenMP "OpenMP ON/OFF" ON)
find_package(OpenMP)
if(OpenMP_FOUND AND ${ENABLE_OpenMP})
  set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS})
endif()
#------------
# Generate pkg-config "pc" file
configure_file(modernfortran-libs.pc.cmake.in modernfortran-libs.pc @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/modernfortran-libs.pc
  DESTINATION lib/pkgconfig)
#------------
# Generate Documentation
# Check Doxygen program
find_package(Doxygen REQUIRED dot)
if(DOXYGEN_FOUND)
  #--Doxygen configuration
  set(DOXY_IN ${CMAKE_CURRENT_SOURCE_DIR}/docs/Doxyfile.cmake.in)
  set(DOXY_OUT ${CMAKE_CURRENT_BINARY_DIR}/docs/Doxyfile)
  configure_file(${DOXY_IN} ${DOXY_OUT} @ONLY)
  #--generate target
  add_custom_target(doxygen 
    COMMAND Doxygen::doxygen ${DOXY_OUT}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/docs
    COMMENT "Generationg API documenttation with Doxygen"
    VERBATIM
    )
else()
  MESSAGE(WARNING "doxygen binary couldn't be found")
endif()

add_subdirectory(src)
