# Text mining con los Simpson

> Idea de R-Ladies Madrid, Verónica García (@yryaa) y Claudia Guirao (@claudiaguirao), 17/10/2017

A partir del taller de TM de R-ladies Madrid, aplicamos algoritmos de cluster sobre los subtítulos de los Simpson. Si saliera bien, que todavía no, identificaríamos las temáticas de los capítulos. 

Necesito los títulos de los capítulos porque sin ellos los resultados son difícilmente interpretables. 

Los cluster se calculan con el tfidf de cada palabra, previa reducción de dimensionalidad con PCA y estratificación. Para las primeras pruebas, _kmeans_ manda.
