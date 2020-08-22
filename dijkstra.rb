# Author : Furkan BEKAR
# Date : May 2020
#İNFO : Bu algoritma ağaçlarda en küçük maliyeti hesaplamamıza yarayan algoritmadır.
#       Genellikle Google Maps gibi uygulamalarda gideceğimiz yere ulaşmak için en kısa yolu
#       tarif etmek gibi amaçlar için kullanılır.
# ---------------- Dijkstra Short Path Algorithm ----------------

class Graph
Dugumler = Struct.new(:isim, :komsular, :mesafe, :prev)

# Burada yapıcı metodumuzu tanımlıyoruz ve bize gerekli olan değişkenlere atama yapıyoruz.
def initialize(graph)
@koseler = Hash.new{|h,k| h[k]=Dugumler.new(k,[],Float::INFINITY)}
@edges = {}
graph.each do |(v1, v2, mesafe)|
@koseler[v1].komsular << v2
@koseler[v2].komsular << v1
@edges[[v1, v2]] = @edges[[v2, v1]] = mesafe
end
@dijkstra_source = nil
end

# Burada algoritmamızı kurmaya başlıyoruz
def dijkstra(baslangic)
return  if @dijkstra_source == baslangic
q = @koseler.values
q.each do |v|
v.mesafe = Float::INFINITY
v.prev = nil
end
@koseler[baslangic].mesafe = 0
until q.empty?
u = q.min_by {|vertex| vertex.mesafe}
break if u.mesafe == Float::INFINITY
q.delete(u)
u.komsular.each do |v|
vv = @koseler[v]
if q.include?(vv)
alt = u.mesafe + @edges[[u.isim, v]]
if alt < vv.mesafe
vv.mesafe = alt
vv.prev = u.isim
end
end
end
end
@dijkstra_source = baslangic
end

# Burada ise bize en kısa yolu bulmamızı sağlayacak olan yardımcı fonksiyonumuzu tanımlıyoruz.
def shortest_path(baslangic, target)
dijkstra(baslangic)
path = []
u = target
while u
path.unshift(u)
u = @koseler[u].prev
end
return path, @koseler[target].mesafe
end

def to_s
"#<%s koseler=%p edges=%p>" % [self.class.isim, @koseler.values, @edges]
end
end

# Burada ise algoritmamızı TEST ediyoruz
g = Graph.new([ [:ankara, :kayseri, 350],
                [:ankara, :sivas, 450],
                [:ankara, :antalya, 700],
                [:kayseri, :sivas, 500],
                [:kayseri, :igdir, 750],
                [:sivas, :igdir, 550],
                [:sivas, :antalya, 100],
                [:igdir, :van, 300],
                [:van, :antalya, 450],
              ])

baslangic, varis = :ankara, :van
yol, mesafe = g.shortest_path(baslangic, varis)
puts "En kısa yol #{baslangic} dan #{varis} uzaklık ise #{mesafe} km dir:"
puts yol.join(" -> ")

gets.chomp
