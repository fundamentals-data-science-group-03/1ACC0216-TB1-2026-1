# Hotel Booking Demand - Fundamentos de Data Science

## Objetivo del trabajo
Realizar un análisis exploratorio de un conjunto de datos (EDA) para encontrar patrones de comportamiento, generando visualizaciones, preparando los datos y extrayendo conclusiones iniciales utilizando R/RStudio como herramienta de software.

## Alumnos participantes
- Belledonne Espinoza, Claudia Valeria
- Carrasco Betancourt, Chris
- Rivera Hernandez, Gabriel Omar
- Elera Rodríguez, Mauricio Elera

## Breve descripción del dataset
El conjunto de datos utilizado se denomina **"Hotel booking demand"**. Consiste en los registros de reservas de dos hoteles ubicados en Portugal: un hotel en la región de Algarve (Resort Hotel) y otro en la ciudad de Lisboa (City Hotel). 

Los datos abarcan desde el 1 de julio de 2015 hasta el 31 de agosto de 2017, e incluyen variables detalladas como:
- Cuándo se realizó la reserva (Lead time, fechas de llegada).
- La duración de la estadía (días entre semana y fines de semana).
- La cantidad de huéspedes (adultos, niños y bebés).
- La cantidad de espacios de estacionamiento disponibles requeridos.
- Información de canales de distribución y estado de la reserva (cancelaciones).

*Nota: Para fines académicos, este conjunto original ha sido modificado incorporando ruido, valores nulos (NA) y datos atípicos (outliers) para aplicar técnicas de pre-procesamiento.*

## Conclusiones

- **Preferencia de establecimiento:** Existe una marcada preferencia por el City Hotel frente al Resort Hotel entre las reservas no canceladas.
- **Tendencia y estacionalidad:** La demanda aumentó con el paso del tiempo (tendencia creciente anual) y presenta un comportamiento estacional con picos a mediados de año y bajas a inicios y fines de año.
- **Meses de mayor y menor flujo:** Julio y agosto concentran la mayor cantidad de reservas (temporada alta), mientras que enero y diciembre presentan la menor demanda (temporada baja).
- **Variabilidad de estadía:** City Hotel concentra estadías cortas con baja variabilidad, mientras que Resort Hotel muestra estadías más largas y dispersas.
- **Perfil del huésped:** Ambos hoteles tienen mayoría abrumadora de "Solo Adultos" (85-90%), pero Resort Hotel presenta una proporción ligeramente superior de reservas con menores.
- **Requerimientos adicionales:** Aproximadamente 1 de cada 10 reservas requiere al menos un espacio de estacionamiento.
- **Análisis de cancelaciones:** Agosto, julio y mayo (en orden descendente) son los meses con mayor número absoluto de cancelaciones, coincidiendo con los periodos de alta demanda.
- **Fidelidad y duración:** Huéspedes repetitivos tienen estadías cortas; huéspedes nuevos tienen estadías más largas, especialmente en el Resort Hotel.

## Licencia
Este proyecto ha sido desarrollado con fines estrictamente académicos para el curso 1ACC0216 - Fundamentos de Data Science en la Universidad Peruana de Ciencias Aplicadas (UPC). El contenido, código y análisis presentados en este repositorio son propiedad de los autores mencionados en la sección de participantes. Se permite su uso y consulta exclusivamente con fines educativos y de referencia, siempre que se otorgue el crédito correspondiente a los autores originales y a la institución académica.