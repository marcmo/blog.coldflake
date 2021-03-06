$:.unshift File.dirname(__FILE__)
require 'rake/clean'
require 'optional'
require 'rubygems'

Port=8888
out="bin"
haky="#{out}/coldflake"
input="coldflake.hs"
CLEAN.include('**/*.hi','**/*.o')
CLOBBER.include(out)

directory out

task :checkSass do
  found = false
  Gem::Specification.find_all do |gem|
    found = found || (gem.name == "sass")
  end
  raise "no sass compiler found!" unless found
end

desc 'build site'
file haky => FileList.new("**/*.hs") << out << :checkSass do
  sh "ghc --make #{input} -outputdir bin -o #{haky}"
end

desc 'guard site'
task :guard do
  sh "bundle exec guard"
end

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
  sh "pyweb #{Port}"
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
      sh "git push blog master:master"
    end
  end
end

desc 'create new post with rake newPost["my new post"]'
task :newPost, [:name] do |t,args|
  t = Time.now
  name = args[:name].gsub!(/\s/,'-')
  puts "creating next post: #{name}"
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

desc 'clean up css'
task :cleanCSS, [:url] do |t,args|
  url = args[:url]
  sh "./run_mincss #{url}"
end
