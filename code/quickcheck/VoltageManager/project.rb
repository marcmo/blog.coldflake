cxx_configuration do
  includes = ['common','Logger','bsp'].map { |i| "../#{i}/include"}
  
  source_lib 'voltageManager',
    :sources => FileList['src/voltage/**/*.cpp'] - FileList["src/voltage/VoltageCondition.cpp"],
    :includes => ['include'] + includes,
    :dependencies => ['testUtils_common']
end


