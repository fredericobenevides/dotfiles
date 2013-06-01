require 'fileutils'

def copy_file(options)
  raise ArgumentError, 'You need to specify ":from" as option' unless options[:from]
  raise ArgumentError, 'You need to specify ":to" as option'   unless options[:to]
  
  from_file = "packages/#{options[:from]}"
  to_file =   "#{File.expand_path(options[:to])}"
    
  puts %Q(==> Copying file from "#{from_file}" to "#{to_file}")
  FileUtils.cp from_file, to_file
end

def copy_folder(options)
  raise ArgumentError, 'You need to specify ":from" as option' unless options[:from]
  raise ArgumentError, 'You need to specify ":to" as option'   unless options[:to]
  
  from_file = "packages/#{options[:from]}"
  to_file =   "#{File.expand_path(options[:to])}"
    
  puts %Q(==> Copying folder from "#{from_file}" to "#{to_file}")
  FileUtils.cp_r from_file, to_file
end