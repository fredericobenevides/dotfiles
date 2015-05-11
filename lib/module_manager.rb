require 'fileutils'

class ModuleManager
  def initialize(module_name)
    @module_name = module_name
    @list_execution = []
  end

  def next_execution(clazz)
    @list_execution << clazz
  end

  def execute_all
    puts '########################################'
    puts "\t\t#{@module_name}"
    puts '########################################'

    @list_execution.each do |le|
      le.execute
    end

    puts "\n\n"
  end
end

class Runner
  def initialize(command)
    @command = command
  end

  def execute
    puts %Q{\n---> Running the command: \n\t"#{@command}"}

    output = system "#{@command}"
    if output
      puts '   * Command status: Success'
    else
      puts '   ! Command status: Error'
    end
  end

  def to_s
    @command
  end
end

class LinkManager
  def initialize(opts = {})
    modules_path = File.dirname(__FILE__) + '/../modules/'

    @from_path = File.expand_path(modules_path + opts[:from])
    @to_path = File.expand_path opts[:to]

    @make_hidden = opts[:make_hidden]
    @link_it = opts[:link_it]
  end

  def execute
    Dir.glob(@from_path).each do |from_path|
      to_path = make_to_path from_path, @to_path, @make_hidden

      if @link_it
        execute_link from_path, to_path
      else
        execute_unlink from_path, to_path
      end
    end
  end

  def execute_link(from_path, to_path)
    puts %Q{\n---> Linking from "#{from_path}" to "#{to_path}"}

    begin
      output = FileUtils.symlink from_path, to_path
      puts '   * Successfully linked'
    rescue
      puts '   ! There is a file already in the same path. Cannot link!'
    end
  end

  def execute_unlink(from_path, to_path)
    puts %Q{\n---> Unlinking #{to_path}" that was linked to "#{from_path}"}

    begin
      output = FileUtils.safe_unlink to_path
      puts '   * Successfully unlinked'
    rescue
      puts '   ! Error while unlinking'
    end
  end

  def make_to_path(from, to, make_hidden)
    to_path = to
    if make_hidden
      from_file = File.basename from
      to_path = File.join(to_path, ".#{from_file}")
    end
    to_path
  end

end

class Description
  def initialize(description)
    @description = description
  end

  def execute
    puts %Q{\n========> "#{@description}"}
  end
end

lambda {
  @module_manager = nil

  define_method :install do |module_name, &block|
    @module_manager = ModuleManager.new module_name
    block.call
    @module_manager.execute_all
  end

  define_method :uninstall do |module_name, &block|
    install module_name, &block
  end

  define_method :run do |command|
    runner = Runner.new command
    @module_manager.next_execution runner
  end

  define_method :link do |options|
    options.merge!(link_it: true)

    linker = LinkManager.new options
    @module_manager.next_execution linker
  end

  define_method :unlink do |options|
    options.merge!(link_it: false)

    linker = LinkManager.new options
    @module_manager.next_execution linker
  end

  define_method :description do |description|
    description = Description.new description
    @module_manager.next_execution description
  end

  define_method :when_os do |os, &block|
    if OS.send "#{os}?"
      block.call
    end
  end
}.call
