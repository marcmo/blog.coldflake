require 'cxxproject'

BUILD_DIR = "VoltageManagerLibLinuxRake"
unittest_flags = {
  :DEFINES => ['UNIT_TEST','USING_GTEST'],
  :FLAGS => "-O0 -g3 -Wall"
}
toolchain = Provider.modify_cpp_compiler("GCC", unittest_flags)
projects = ['project.rb', '../testUtils/project.rb']
CxxProject2Rake.new(projects, BUILD_DIR, toolchain)
