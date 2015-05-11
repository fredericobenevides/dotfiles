#!/usr/bin/env ruby

$LOAD_PATH << File.join(File.dirname(__FILE__))

require 'lib/os_detection'
require 'lib/module_manager'

require 'modules_setup'
Dir.glob("modules/**/*.rb") do |package|
  require package
end

system 'clear'

MODULES.reverse.each do |m|
  Kernel.send "uninstall_#{m}"
end
