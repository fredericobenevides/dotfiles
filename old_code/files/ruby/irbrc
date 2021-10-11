require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-history"

if defined? Rails
  require 'hirb'
end

require 'awesome_print'
require 'interactive_editor'
