cxx_configuration do
  deps = BinaryLibs['pthread','dl','gtest','gtest_main']
  exe "testme",
    :sources => FileList['death_test.cpp','static_asserts.cpp'],
    :includes => ['include'],
    :dependencies => deps
end
