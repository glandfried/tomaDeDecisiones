# 6.6
# En una universidad, el 15% de los estudiantes estudia ciencias sociales
# el 58% de los estudiantes son de grado
# y el 19% de los estudiantes de grado estudia ciencias sociales. ¿Cuál 
# es la probabilidad de que un estudiantes
# de ciencias sociales elegido al azar sea de grado?

p_CS <- 0.15
p_grado <- 0.58
p_CS_dado_grado <- 0.19
p_grado_dado_CS <- p_CS_dado_grado * p_grado/p_CS

# 6.7

# 6.7.A
# Hacer un esquema gráfico del modelo generativo para cada esquema,
# usando las siguientes variables: I 1 ,
# I2 , I3 , I4 y I5 para las mediciones/observaciones, y para 
# el estado del mundo s (si es uno solo) o s 1 , s 2 , s 3 ,
# s 4 y s 5 . Algunas variables pueden aparecer más de una vez.
p_s <- 0.5

print("
Modelo (M1)
s1 -> I1
s2 -> I2
...
sn -> In

Modelo (M2)
    ______________
   |  0 < i < n+1 |
s -|-> Ii         |
   |______________|
")

# Paso 2: Inferencia 
# En inferencia, los dos escenarios pasan a ser escenario hipotéticos
# H1 y H2. La inferencia involucra likelihoods y priors. El likelihood de un escenario es 
# la probabilidad de las observaciones sensoriales si el escenario es el correcto.

# 6.7.B,C,E,D
print("
Likelihood 1: p(I | s, M1)
Likelihood 2: p(I | s, M2)

Notar la diferenca repecto de
Evidencia 1: p(I | M1)
Evidencia 2: p(I | M2)
que es el likelihood de los modelos.

Los likelihoods no necesariamenre suman 1 porque la variable
es el condicional. Se evaluan diferentes distribuciones de
probabilidad en el mismo punto.

El likelihood es la verosimilitud de la hip\'otesis,
cuan probable es el dato dado la hipótesis
")

# 6.7.F
p_m1 <- 0.66
p_m2 <- 0.33

# 6.8
# En una bolsa hay 4 pelotas. Las pelotas pueden ser rojas o negras pero no se sabe cuántas 
# hay de cada color. Se extraen tres pelotas, de a una y reemplazando en la bolsa después de 
# cada extracción, y se obtiene la secuencia (roja, negra, roja). ¿Cuál es el número más 
# probables de pelotas de cada color? Después se extrae una cuarta y es roja. ¿Cómo cambia 
# el cálculo? Calcular la plausibilidad (probabilidad) de cada posible “hipótesis” 
# sobre el contenido de la bolsa.


