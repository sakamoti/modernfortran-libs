#-----------------
set(modernfortran-libs_sources 
  mod_io.f90
  mod_doubly_linkedlist.f90
  )
#-----------------
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modernfortran-libs)
add_library(modernfortran-libs ${modernfortran-libs_sources})
#target_link_libraries(modernfortran-libs 
#    ${FITPACK_LIBRARY} 
#    ${MINPACK_LIBRARY} 
#    ${FFTW3_LIBRARY} 
#    ${LAPACK_LIBRARIES} 
#    ${BLAS_LIBRARIES} 
#  )
#target_include_directories(modernfortran-libs PUBLIC ${FFTW3_INCLUDE_DIR})
#---
# get module directory path
get_property(fmods TARGET modernfortran-libs PROPERTY Fortran_MODULE_DIRECTORY)
include_directories(${fmods})

#-----------------
# define install file and destination of binary files
install(TARGETS modernfortran-libs 
  LIBRARY DESTINATION lib 
    PERMISSIONS 
      OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE
      OWNER_READ GROUP_READ WORLD_READ
  ARCHIVE DESTINATION lib
  )
install(DIRECTORY ${fmods} DESTINATION include)

#-----------------
# test programs folder
#add_subdirectory(example)
