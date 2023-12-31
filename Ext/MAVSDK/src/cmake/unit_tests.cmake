find_package(jsoncpp REQUIRED)

add_executable(unit_tests_runner
    ${UNIT_TEST_SOURCES}
)

target_compile_definitions(unit_tests_runner PRIVATE FAKE_TIME=1)

set_target_properties(unit_tests_runner
    PROPERTIES COMPILE_FLAGS ${warnings}
)

target_link_libraries(unit_tests_runner
    mavsdk
    CURL::libcurl
    JsonCpp::JsonCpp
    gtest
    gtest_main
    gmock
)

target_include_directories(unit_tests_runner
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../src/mavsdk/core
    PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../src/mavsdk/core
)
target_include_directories(unit_tests_runner SYSTEM
    PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/third_party/include
    PRIVATE ${MAVLINK_HEADERS}
)

if (MSVC AND BUILD_SHARED_LIBS)
    target_compile_definitions(unit_tests_runner PRIVATE -DGTEST_LINKED_AS_SHARED_LIBRARY)
    set_target_properties(unit_tests_runner
        PROPERTIES COMPILE_FLAGS "${warnings} /wd4275"
    )
endif()

add_test(unit_tests unit_tests_runner)
