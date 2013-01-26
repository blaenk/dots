require 'rexml/document'
require 'base64'

doc = REXML::Document.new(File.open(ARGV[0]).read)

key_bl = %w{DisableANSIColor}
pair_bl = %w{Font}

keys = doc.elements.to_a('plist/dict/key')
          .map {|key| key.text}
          .select {|k| pair_bl.include?(k) || !key_bl.include?(k) && /Color$/ =~ k}

data = doc.elements.to_a('plist/dict/data').map {|data| data.text}

zipped = keys.zip(data).select {|pair| !pair_bl.include?(pair[0])}
                       .map {|pair| [pair[0], Base64.decode64(pair[1])]}

colors = zipped.map do |pair|
  name = pair[0]
  decoded = pair[1]
  color = decoded.match(/([01](\.[0-9]+)? ?){3}/)[0].split.map {|s| Float(s)}

  # normalized
  normalized = color.map {|component| component.to_s}.join(" ")
  
  # to rgb
  rgb = color.map {|component| (component * 255).to_i}.join(" ")

  # to hex rgb
  hex = rgb.split.map {|component| component.to_i.to_s(16)}.join

  output = "Normalized: #{normalized}\nRGB: #{rgb}\nHEX: ##{hex}"
  [name, output]
end

colors.each {|color| puts color[0]; puts color[1]; puts }
