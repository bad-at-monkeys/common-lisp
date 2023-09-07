(defvar quality-table
  '((large       .  size)
    (small       .  size)
    (red         .  color) 	 
    (green       .  color)
    (blue        .  color)
    (shiny       .  luster)
    (dull        .  luster)
    (metal       .  material)
    (plastic     .  material)
    (cube        .  shape)
    (sphere      .  shape)
    (pyramid     .  shape)
    (four-sided  .  shape)))


(defvar rooms

  '((living-room        (north front-stairs)
			(south dining-room)
			(east kitchen))

    (upstairs-bedroom   (west library) 
			(south front-stairs))

    (dining-room        (north living-room) 
			(east pantry)
			(west downstairs-bedroom))

    (kitchen            (west living-room)
			(south pantry))

    (pantry             (north kitchen)
			(west dining-room))

    (downstairs-bedroom (north back-stairs)
			(east dining-room))

    (back-stairs        (south downstairs-bedroom)
			(north library))

    (front-stairs       (north upstairs-bedroom)
			(south living-room))

    (library            (east upstairs-bedroom)
			(south back-stairs))))

