module Formas (
  Forma (
    Esfera,
    Cilindro,
    Cone,
    TroncoCone,
    EsferoideOblato,
    EsferoideProlato
  ),
  Raio,
  Altura,
) where

data Forma = Esfera Raio
           | Cilindro Raio Altura
           | Cone Raio Altura
           | TroncoCone Raio Raio Altura
           | EsferoideOblato Raio Raio
           | EsferoideProlato Raio Raio
  deriving Show

type Raio = Float
type Altura = Float

area :: Forma -> Float
area (Esfera r) = 4 * pi * r ^ 2
area (Cilindro r a) = 2 * pi * r ^ 2 + areaLateral (Cilindro r a)
area (Cone r a) = pi * r ^ 2 + areaLateral(Cone r a)
area (TroncoCone r1 r2 a) = pi * (r1 ^ 2 + r2 ^ 2) + areaLateral (TroncoCone r1 r2 a)
area (EsferoideOblato a b) = 2 * pi * a ^ 2 + (b ^ 2 / excentricidade a b) * log (1 + excentricidade a b) / (1 - excentricidade a b)
area (EsferoideProlato a b) = 2 * pi * b ^ 2 + 2 * pi * a * b / excentricidade a b * asin (excentricidade a b)

volume :: Forma -> Float
volume (Esfera r) = r / 3 * area (Esfera r)
volume (Cilindro r a) = r / 2 * areaLateral (Cilindro r a)
volume (Cone r a) = pi / 3 * r ^ 2 * a
volume (TroncoCone r1 r2 a) = pi / 3 * a * (r1 ^ 2 + r2 ^ 2 + r1 * r2)
volume (EsferoideOblato a b) = 4 / 3 * pi * a ^ 2 * b
volume (EsferoideProlato a b) = 4 / 3 * pi * a * b ^ 2

areaLateral :: Forma -> Float
areaLateral (Cilindro r a) = 2 * pi * r * a
areaLateral (Cone r a) = pi * r * sqrt(r ^ 2 + a ^ 2)
areaLateral (TroncoCone maior menor a) = pi * (maior + menor) * sqrt(a ^ 2 + maior ^ 2 + menor ^ 2)

excentricidade :: Raio -> Raio -> Float
excentricidade a b = sqrt(a ^ 2 + b ^ 2) / a
