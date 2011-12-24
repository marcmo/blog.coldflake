
module Utils
  def self.optional_package(block1, block2)
    begin
      block1.call
    rescue LoadError => e
      puts e
      block2.call if block2
    end
  end
end
