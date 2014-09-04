$:.unshift File.join(File.dirname(__FILE__))
require 'rake'
require 'rakefile.rb'

guard 'rake', :task => 'build' do
  watch(/(posts\/.*|js\/.*|css\/.*|images\/.*|templates\/.*|.*\.md)/)
end

