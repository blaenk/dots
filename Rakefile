require 'rake'

desc "link files"
task :link do
  Dir['*'].each do |file|
    next if %[Rakefile README.md Blaenk.terminal].include? file

    link file
  end
end

def link(file)
  src = File.join(Dir.pwd, "#{file}")
  dest = File.join(ENV['HOME'], ".#{file}")

  if !File.exists?(dest)
    File.symlink(src, dest)
    puts "LINKED: #{src} => #{dest}"
  else
    if File.symlink?(dest)
      link = File.readlink(dest)

      if link == src
        puts "SKIPPED: #{src} => #{dest}"
      else
        puts "EXISTING SYM: #{dest} => #{link}"
      end
    elsif
      puts "EXISTING NON-SYM: #{dest}"
    end
  end
end

