require 'matrix'
require 'rexml/document'
require 'set'

w = 224
board = IO.readlines('pacman.pbm')[3..-1].map(&:chomp).join.split(//).each_slice(w).to_a
m=Matrix.rows(board.map{|l|l.map{|c|1 if c=='1'}})

def neighbor_coords(coord, row_size, column_size)
  r, c = coord
  [r - 1, r, r + 1].product([c - 1, c, c + 1]).select{|r_new,c_new|
    r_new >= 0 &&
    c_new >= 0 &&
    r_new < row_size &&
    c_new < column_size &&
    [r_new, c_new] != [r, c]
  }
end

def next_neighbor_coords(m, coord, visited)
  neighbor_coords(coord, m.row_size, m.column_size).select {|coord| m[*coord] && !visited.include?(coord)}
end

def addable(m, coord, visited)
  coord && m[*coord] && !visited.include?(coord)
end

def find_line_coord_lists(m)
  lines = []
  visited = Set.new
  m.each_with_index{
    |e,r,c|
    coord = [r,c]
    next unless addable(m, coord, visited)
    first_run = true
    other_neighbor = nil
    while addable(m, coord, visited)
      if first_run
        lines << []
        other_neighbor = next_neighbor_coords(m, coord, visited)[1]
        first_run = false
      end
      lines[-1] << coord
      visited.add(coord)
      if coord == other_neighbor
        $polygons[lines.size - 1] = true
      end
      coord = next_neighbor_coords(m, coord, visited)[0]
    end
    while addable(m, other_neighbor, visited)
      lines[-1].unshift(other_neighbor)
      visited.add(other_neighbor)
      other_neighbor = next_neighbor_coords(m, other_neighbor, visited)[0]
    end
  }
  lines
end

def endpoints(coords)
  return coords if coords.size <= 1
  dir_prev = [coords[1][0] - coords[0][0], coords[1][1] - coords[0][1]]
  coords.chunk_while{|coord0, coord1|
    dir = [coord1[0] - coord0[0], coord1[1] - coord0[1]]
    p = dir == dir_prev
    dir_prev = dir
    p
  }.flat_map{|chunk|chunk.one? ? chunk : [chunk[0], chunk[-1]]}
end

def to_svg(m, lines)
  doc = REXML::Document.new('<?xml version="1.0" encoding="UTF-8"?><svg xmlns="http://www.w3.org/2000/svg"/>')
  doc.root.add_attribute('width', m.column_size)
  doc.root.add_attribute('height', m.row_size)
  # doc.root.add_element 'rectangle', {x: 1, y: 1, width: 2, height: 3}
  lines.each.with_index{|coords, i|
    doc.root.add_element ($polygons[i] ? 'polygon' : 'polyline'),
      {'fill' => 'none', 'stroke' => 'black', 'points' => coords.map{|coord|"#{coord[1]},#{coord[0]}"}.join(' ')}
  }
  doc
end

$polygons = []
lines = find_line_coord_lists(m).map{|coords|endpoints(coords)}
output = ''
to_svg(m, lines).write(:output => output)
File.open('pacman.svg', 'w') {|f|
  f.puts(output)
}
