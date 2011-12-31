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

desc 'compile a post with hastache'
task :hastache, [:post] do |t,args|
  runHastache args[:post]
end

desc 'build site'
file haky => FileList.new("**/*.hs") << out do
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

define_syntax_highlighter = lambda do
  require 'nokogiri'
  require 'albino'
  require 'redcarpet'

  def markdown(text)
    options = [:hard_wrap, :filter_html, :autolink, :no_intraemphasis, :fenced_code, :gh_blockcode]
    syntax_highlighter(Redcarpet.new(text, *options).to_html)
  end

  def syntax_highlighter(html)
    doc = Nokogiri::HTML(html)
    doc.search("//pre[@lang]").each do |pre|
      pre.replace Albino.colorize(pre.text.rstrip, pre[:lang])
    end
    doc.to_s
  end
  desc 'test highlighting'
  task :highlight, [:text] do |t,args|
    input = args[:text]
    puts Dir.pwd
    File.open("posts/#{input}", "r") do |f|
      t = f.read
      puts markdown(t)
    end
  end
end

def needsUpdate?(g)
  s = g.status
  res = false
  if (s.untracked.keys + s.changed.keys + s.added.keys + s.deleted.keys).length > 0
    puts "update needed"
    res = true
  end
  res
end
  
desc 'deploy latest generated site to server'
task :deploy => :rebuild do
  puts "deploying..."
  require 'git'
  g = Git.open ('.')
  cd "deploy" do
    dg = Git.open ('.')
    rm_rf "_site"
    cp_r "../_site","."
    if needsUpdate? dg
      sh "git add -u ."
      sh "git add ."
      sha = dg.object('HEAD').sha[0..6]
      sh "git commit -m 'publish site for commit #{sha}'"
      sh "git push coldflake"
    end
  end
end

Utils.optional_package(define_syntax_highlighter, nil)

