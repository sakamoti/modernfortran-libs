#----------
# copy data files
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test.dat.in  DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
# rename data files
file(RENAME ${CMAKE_CURRENT_BINARY_DIR}/test.dat.in ${CMAKE_CURRENT_BINARY_DIR}/test.dat)

# get module directory path
get_property(fmods TARGET modernfortran-libs PROPERTY Fortran_MODULE_DIRECTORY)
include_directories(${fmods})

#-----------
add_executable(test_dlmreadwrite test_dlmreadwrite.f90)
target_link_libraries(test_dlmreadwrite modernfortran-libs)
target_include_directories(test_dlmreadwrite PUBLIC ${fmods})
#--
add_test(NAME test_dlmreadwrite COMMAND test_dlmreadwrite WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
