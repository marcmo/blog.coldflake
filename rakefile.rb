$:.unshift File.dirname(__FILE__)
require 'rake/clean'
require 'optional'

Port=8888
out="bin"
haky="#{out}/coldflake"
input="coldflake.hs"
# input="compilersample.hs"
CLEAN.include('**/*.hi','**/*.o')
CLOBBER.include(out)

directory out
def runHastache post
  sh "runghc hastacheProcessing.hs #{post} | pbcopy"
end

task :checkSass do
  found = false
  available_gems = Gem::Specification.find_all do |gem|
    found = found || (gem.name == "sass")
  end
  raise "no sass compiler found!" unless found
end
desc 'compile a post with hastache'
task :hastache, [:post] do |t,args|
  runHastache args[:post]
end

desc 'build site'
file haky => FileList.new("**/*.hs") << out << :checkSass do
  begin
    sh "ghc --make #{input} -outputdir bin -o #{haky}"
  rescue Exception => e
    puts "buiding #{haky} was not possible: #{e}"
  end
end
task :hastache => [:code2html]

task :clean_hakyll do
  begin
    sh "./#{haky} clean"
  rescue Exception => e
    puts "cleaning hakyll was not possible: #{e}"
  end
end
task :clean => [:clean_hakyll]

desc "run webserver on port #{Port} for preview"
task :preview => :rebuild do
  # sh "python -m SimpleHTTPServer #{Port}"
  sh "./#{haky} preview #{Port}"
end

desc 'incrementally build site'
task :build => haky do
  sh "./#{haky} build"
end

desc 'completly rebuild site'
task :rebuild => [haky] do
  sh "./#{haky} rebuild"
end

task :default => [:preview]

task :create_file_tasks do
  puts "inside code2html_intern"
  file_tasks = []
  Dir.glob("code/*") do |p|
    mkdir "#{p}/html" unless Dir.exists?("#{p}/html")
    to_take = Dir.glob("#{p}/*.{rb,hs,h,cpp,c,lua,bf}")
    leave = Dir.glob("#{p}/*.*") - to_take
    file_tasks << to_take.collect do |f|
      n = File.basename(f).chomp(File.extname(f))
      file "#{p}/html/#{n}.html" => f do |t|
        sh "pygmentize -o #{p}/html/#{n}.html #{f}"
      end
    end
  end
  file_tasks.flatten
end

task :code2html_intern => :create_file_tasks

desc 'generate html from source code'
task :code2html => :code2html_intern

def needsUpdate?
  st = `git status --porcelain`
  puts st
  st.lines.count > 0
end
  
desc 'deploy latest generated site to server'
task :deploy => :rebuild do
  sha = `git show -s --pretty=format:%T master`[0..6]
  puts "deploying...#{sha}"
  cd "deploy", :verbose=>false do
    rm_rf "_site"
    cp_r "../_site","."
    if needsUpdate?
      msg = "publish site for commit #{sha}, see https://github.com/marcmo/blog.coldflake/commit/#{sha}"
      puts "deployment needed: #{msg}"
      sh "git add -u ."
      sh "git add ."
      sh "git commit -m '#{msg}'"
      sh "git push coldflake"
    end
  end
end


def escape(n)
  escapes = {
    "+" => "Plus"
  }
  n.split("").reduce("") do |acc,x|
    acc << (escapes[x] ? escapes[x] : x)
  end
end
desc 'create new post'
task :newPost, [:post_name] do |t,args|
  name = escape(args[:post_name])
  t = Time.now
  postName = t.strftime("%Y-%m-%d-#{name}.md")
  post = File.join("posts",postName)
  if File.exists?(post)
    puts "post with the name #{postName} already exists!"
  else
    puts "creating......post with the name #{postName}"
    p = File.new(post, "w")
    p.puts "---"
    p.puts "title: #{name.split('-').each{|word| word.capitalize!}.join(' ')}"
    p.puts "description:"
    p.puts "tags:"
    p.puts "---"
  end
end

