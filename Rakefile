require 'rake'

desc "link files"
task :link do
  Dir['*'].each do |file|
    next if %[Rakefile README.md].include? file

    if File.exist?(File.join(ENV['HOME'], ".#{file}"))
      puts %Q{rm #{`echo $HOME`.chomp}/.#{file}}
      system %Q{rm "$HOME/.#{file}"}
    end

    link file

    puts
  end
end

def link(file)
  puts %Q{ln -s #{`echo $PWD`.chomp}/#{file} #{`echo $HOME`.chomp}/.#{file}}
  system %Q{ln -s "$PWD/#{file}" "$HOME/.#{file}"}
end
