# prog_soc_colima_pobreza_multidimensional
Visualización de la distribución de los programas sociales en Colima considerando los cuadrantes de bienestar de Coneval.
Por Zatara.

Se utilizaron como fuentes para las estimaciones los microdatos de las ediciones 2018 y 2020 de la ENIGH, del INEGI y la base de datos de pobreza multidimensional producida por Coneval para los mismos años. En el script del programa de visualización se encuentran los enlaces a los archivos fuente.

El procedimiento empleado fue emparejar las observaciones de la base de datos de Coneval con las observaciones de la ENIGH mediante las variables llave folioviv, foliohog y numren, para poder observar las transferencias en los cuadrantes de bienestar estimados por Coneval. Después, se utilizó la paquetería srvyr para realizar estimaciones para la población colimense. Los coeficientes de variación de las estimaciones se reportan en las tablas correspondientes a cada gráfica.
