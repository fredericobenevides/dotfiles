#!/usr/bin/env ruby

$LOAD_PATH << File.join(File.dirname(__FILE__))

require 'lib/os_detection'
require 'lib/module_manager'

require 'modules_setup'

system 'clear'

MODULES.reverse.each do |m|
  load "modules/#{m}/setup.rb"
  Kernel.send 'uninstall_module'
end
