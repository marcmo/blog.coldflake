require 'rake/clean'

out="out"
exe="#{out}/testexe"
CLEAN.include("*.o","*.hi","*.a","out")
CLOBBER.include(exe)
name="test"
libname="#{out}/libtest.a"
ofiles=FileList.new("c_interface.o")
srcFiles=FileList.new("**/*.h","**/*.cpp","**/*.c")
# ArchFlag="-arch i386"
ArchFlag=""

task :default => [:clean,:run]
directory out 
file "#{out}/c_interface.o" => srcFiles do
  sh "gcc #{ArchFlag} -c cpp/c_interface.cpp -o#{out}/c_interface.o -I../../include -I../../../bsp/include -I../../../common/include -I../../../TestUtils/include"
end

file "#{out}/TimeoutManagerMock.o" => srcFiles do
  sh "gcc #{ArchFlag} -c cpp/TimeoutManagerMock.cpp -o#{out}/TimeoutManagerMock.o -I../../include -I../../../bsp/include -I../../../common/include -I../../../Logger/include"
end

file libname => [out,"#{out}/c_interface.o","#{out}/TimeoutManagerMock.o"] do
  sh "ar -cvq #{libname} #{out}/c_interface.o #{out}/TimeoutManagerMock.o" 
end

desc 'link haskell executable'
file exe => [libname,"../../VoltageManagerLibLinuxRake/libs/libvoltageManager.a"] + Dir.glob("**/*.hs") do
  cd "haskell" do
    sh "ghc --make testrunner.hs -outputdir ../#{out} -L. -L../#{out} -L../../../VoltageManagerLibLinuxRake/libs -ldl -lstdc++ -l#{name} -ltestUtils_common -lvoltageManager -o ../#{exe}"
  end
end

desc 'run program'
task :run => [out,exe] do
  sh "./#{exe}"
  cd out do
    xs = Dir.glob("*.rb")
    xs.each { |f| sh "ruby #{f}" }
  end
end

task :link_c do
  sh "gcc -o ctext ctext.o c_interface.o -ldl"
end

desc "create some sample plots"
task :runPlots => [out] do
  cd "haskell" do
    sh "ghc -O2 -o ../#{out}/plotter -outputdir ../#{out} --make PlotMain.hs -fforce-recomp"
  end
  sh "./#{out}/plotter"
end


