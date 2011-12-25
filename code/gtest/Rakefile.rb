require 'cxxproject'
BuildDir = "BuildDir"

unittest_flags = {
  :DEFINES => ['UNIT_TEST'],
  :FLAGS => "-O0 -g3 -Wall"
}
toolchain = Provider.modify_cpp_compiler("GCC", unittest_flags)
dependent_projects =  ['./project.rb']
CxxProject2Rake.new(dependent_projects, BuildDir, toolchain, './')
