# Polivictimización: Etiquetas de variables

b_etiquetas_corto <-
  tibble::tribble(
    ~ modulo, ~ modulo8, ~variable, ~label,
	     "A",       "A1",   "PA_1", "Robo sin usar la fuerza",
	     "A",       "A1",   "PA_2", "Robo utilizando la fuerza",
	     "A",       "A1",   "PA_3", "Rotura de algo a propósito",
	     "A",       "A2",   "PA_4", "Amenaza o percepción de intento de daño",
	     "A",       "A1",   "PA_5", "Ataque físico con objetos",
	     "A",       "A1",   "PA_6", "Ataque físico sin objetos",
	     "A",       "A2",   "PA_7", "Amenaza por alguna característica",
	     "B",        "B",   "PB_1", "Sentido mal por insulto de adulto cercano",
	     "B",        "B",   "PB_2", "Ataque físico de adulto cercano",
	     "B",        "B",   "PB_3", "Sentido mal por descuido de adultos con quienes vives",
	     "B",        "B",   "PB_4", "Apartado, mantenido alejado o escondido de tu padre o de tu madre",
	     "C",        "C",   "PC_1", "Ataque físico de un solo NNA*",
	     "C",        "C",   "PC_2", "Ataque físico de un grupo NNA*",
	     "C",        "C",   "PC_3", "Sentido mal por insulto de un grupo de NNA*",
	     "C",        "C",   "PC_4", "Imposición a hacer cosas que no quiere por un NNA*",
	     "C",        "C",   "PC_5", "Ataque físico de una pareja romántica",
	     "D",        "D",   "PD_1", "Prácticas sexuales con mayor con consentimiento",
	     "D",        "D",   "PD_2", "Herida de sentimientos por bullying sexual (sin internet)",
	     "D",        "D",   "PD_3", "Obligado a mirar sus partes íntimas por fuerza o sorpresa",
	     "D",        "D",   "PD_4", "Forzado a hacer cosas de carácter sexual por un NNA*",
	     "D",        "D",   "PD_5", "Tocado o intento de toque de partes íntimas por adulto extraño",
	     "D",        "D",   "PD_6", "Tocado o intento de toque de partes íntimas por adulto conocido",
	     "D",        "D",   "PD_7", "Intento o relación sexual completas con penetración",
	     "E",       "E1",   "PE_1", "Robo en casa",
	     "E",       "E1",   "PE_2", "Presenciar violencia",
	     "E",       "E1",   "PE_3", "Presenciar discriminación",
	     "E",       "E1",   "PE_4", "Presenciar ataque físico sin objetos",
	     "E",       "E1",   "PE_5", "Presenciar ataque físico con objetos",
	     "E",       "E2",   "PE_6", "Presenciar ataque físico entre padres",
	     "E",       "E2",   "PE_7", "Presenciar ataque físico de padres a hermanos",
	     "F",        "F",   "PF_1", "Molestia, acoso o rumores por internet",
	     "F",        "F",   "PF_2", "Acoso sexual por internet")

  b_etiquetas_modulo8 <-
  tibble::tribble(
  ~ modulo,~ modulo8, ~label,
        "A",     "A1", "Delitos comunes: ataque físico",
        "A",     "A2", "Delitos comunes: amenazas",
        "B",      "B", "Maltrato por cuidadores",
        "C",      "C", "Maltrato por pares",
        "D",      "D", "Sexuales",
        "E",     "E1", "Indirectas: exposición a la violencia en comunidad",
        "E",     "E2", "Indirectas: exposición a la violencia en familia",
        "F",      "F", "Digitales")