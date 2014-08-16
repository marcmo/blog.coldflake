$:.unshift File.join(File.dirname(__FILE__))
require 'rake'
require 'rakefile.rb'

# guard :shell do
#   watch(/(posts|js|css|images|templates)\/.*/) {|m| `tail #{m[0]}` }
# end

guard 'rake', :task => 'build' do
  watch(/(posts|js|css|images|templates)\/.*/)
end

# Add files and commands to this file, like the example:
#   watch(%r{file/path}) { `command(s)` }
#
guard :shell do
  watch(/(.*).txt/) {|m| `tail #{m[0]}` }
end
