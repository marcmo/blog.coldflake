require 'rake/clean'
require 'benchmark'

InName="Square"
Input="#{InName}.bf"
Inputs=FileList.new("*.bf")

Program="interpreter"
MainHs="interpreter"
TmpFolder = "tmp"
#Profiling
ProfilingExecutable = "for_profiling"
CLEAN.include(TmpFolder,"**/*.o","*.out","**/*.hi","dist",Program,"#{ProfilingExecutable}*")
SrcFiles = FileList.new('**/*.hs')

file Program => SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{Program} -outputdir #{TmpFolder} --make #{MainHs} -threaded -fforce-recomp"
end
desc "build executable"
task :build => [:clean,Program]

desc "run program on data"
file :run => Program do
	sh "time ./#{Program} < #{Input}"
end

taskNr = 0
Inputs.map do |i|
  taskNr = taskNr + 1
  desc "run #{Program} on #{i}"
  file "run#{taskNr}".intern => Program do
    sh "time ./#{Program} < #{i}"
  end
end

file ProfilingExecutable => SrcFiles do
  sh "ghc -O2 -o #{ProfilingExecutable} -outputdir #{TmpFolder} --make #{MainHs} -prof -auto-all -caf-all -fforce-recomp -rtsopts"
end

namespace :prof do
  desc "time profiling for \"#{Input}\""
  task(:t => [:clean,ProfilingExecutable]) { runProfiling("+RTS -p -K100M",false) }
  desc "heap profiling"
  task(:h => [:clean,ProfilingExecutable]) { runProfiling("+RTS -hc -p -K100M",true) }
  desc "allocation-type profiling"
  task(:a => [:clean,ProfilingExecutable]) { runProfiling("+RTS -hc -p -K100M",true) }
  desc "constructor-alloc-type profiling"
  task(:c => [:clean,ProfilingExecutable]) { runProfiling("+RTS -hd -p -K100M",true) }
end

def runProfiling(options,psOutput)
  benchmark = Benchmark.realtime do
    sh "time ./#{ProfilingExecutable} < #{Input} #{options}"      
  end
  puts "computing step took: " + sprintf("%.2f", benchmark)
  if psOutput
    sh "hp2ps -e8in -c #{ProfilingExecutable}.hp"
  end
end

task :default => [:clean, :build]
