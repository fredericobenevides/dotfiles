# Module responsible to detect the OS system
module OS
  def self.linux?
    (/linux/ =~ RUBY_PLATFORM) != nil
  end
  def self.mac?
    (/darwin/ =~ RUBY_PLATFORM) != nil
  end
end
