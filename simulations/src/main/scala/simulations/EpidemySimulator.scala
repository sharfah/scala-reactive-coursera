package simulations

import math.random
import com.sun.org.apache.bcel.internal.generic.POP

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate: Int = 1
    val transmissionRate: Int = 40
    val deathRate: Int = 25
    val initialInfected: Int = prevalenceRate * population / 100

    val numMoveDays: Int = 5
    
    // extensions
    val enableAirTraffic = false
    val enableReducedMobility = false
    val enableVaccine = false
    val numVaccinated: Int = 5 * population / 100    
  }

  import SimConfig._
  
  // In the beginning, 300 people are equally distributed among the rooms.
  val persons: List[Person] = for (i <- (1 to population).toList) yield new Person(i) // to complete: construct list of persons
  
  // a prevalence rate of 1%
  // By prevalence rate, we mean that a certain portion of the population is infected to begin with.
  for (i <- 1 to initialInfected) {
    var hasInfected = false
    while (!hasInfected) {
      val randomPerson = persons(randomBelow(population))
      if (!randomPerson.infected) {
        randomPerson.setInfected
        hasInfected = true
      }
    }
  }
  
  // The Chosen Few Act extension: 5% of people (VIPs such as pop singers, football players, etc.) 
  // are given vaccines when first created. They never become infected.
  if (enableVaccine) {
    for (i <- 1 to numVaccinated) {
      var hasVaccinated = false
      while (!hasVaccinated) {
        val randomPerson = persons(randomBelow(population))
        if (!randomPerson.vaccinated && !randomPerson.infected) {
          randomPerson.vaccinated = true
          hasVaccinated = true
        }
      }
    }
  }
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var vaccinated = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def setInfected {
      infected = true

      // After 6 days of becoming infected, a person becomes sick and is therefore visibly infectious.
      afterDelay(6)(sick = true)

      // After 14 days of becoming infected, a person dies with a probability of 25%. Dead people do not move, but stay visibly infectious.
      afterDelay(14)(if (randomBelow(100) + 1 <= 25) dead = true)

      // After 16 days of becoming infected, a person becomes immune. He is no longer visibly infectious, but remains infectious.
      // An immune person cannot get infected.
      afterDelay(16)(if (!dead) { sick = false; immune = true })

      // After 18 days of becoming infected, a person turns healthy. 
      // He is now in the same state as he was before his infection, which means that he can get infected again.
      afterDelay(18)(if (!dead) { sick = false; immune = false; infected = false })
    }

    def isRoomVisiblyInfected(row: Int, col: Int) = persons.exists(p => row == p.row && col == p.col && (p.sick || p.dead))
    def isRoomInfected(row: Int, col: Int) = persons.exists(p => row == p.row && col == p.col && (p.sick || p.dead || p.infected))
    
    //
    // to complete with simulation logic
    //
    def moveRoom {
      if (!dead) {

        // Air traffic extension: when a person decides to move, she will choose to take the airplane with a probability of 1%, 
        // thereby moving to a random room in the grid (rooms with visibly infected people are not avoided)
        var hasMoved = false
        if (enableAirTraffic && randomBelow(100) + 1 == 1) {
          var found = false
          while (!found) {
            val airRow = randomBelow(roomRows)
            val airCol = randomBelow(roomColumns)
            if (!(airRow == row && airCol == col)) {
              row = airRow
              col = airCol
              found = true
              hasMoved = true
            }
          }
        } else {
          val neighbouringRooms = List(
            (if (row - 1 < 0) roomRows - 1 else row - 1, col), // above
            (if (row + 1 >= roomRows) 0 else row + 1, col), // below
            (row, if (col - 1 < 0) roomColumns - 1 else col - 1), // left
            (row, if (col + 1 >= roomColumns) 0 else col + 1)) // right

          /*A person avoids rooms with sick or dead (visibly infectious) people. 
	      This means that if a person is surrounded by visibly infectious people, he does not change position; 
	      however, he might change position the next time he tries to move 
	     (for example, if a visibly infectious person moved out of one of the neighbouring rooms or became immune).*/
          val uninfectedRooms = neighbouringRooms.filter(t => !isRoomVisiblyInfected(t._1, t._2))
          if (!uninfectedRooms.isEmpty) {
            val (destRow, destCol) = uninfectedRooms(randomBelow(uninfectedRooms.size))
            row = destRow
            col = destCol
            hasMoved = true
          }
        }
        
        /* When a person moves into a room with an infectious person he might get infected according to the transmissibility rate, 
		 * unless the person is already infected or immune. A person cannot get infected between moves (this is slightly unrealistic, 
		 * but will simplify your implementation).
		 */
        if (hasMoved && isRoomInfected(row, col) && !infected && !immune && !vaccinated && (randomBelow(100) + 1 <= transmissionRate)) {
          setInfected
        }

        // Reduce Mobility Act extension: The mobility of people is decreased by half. 
        // The mobility of a visibly infected person is further reduced by half.
        val timeToMove = if (enableReducedMobility) numMoveDays * (if (sick) 4 else 2) else numMoveDays
        
        afterDelay(randomBelow(timeToMove) + 1)(moveRoom)
      }      
    }

    val timeToMove = if (enableReducedMobility) numMoveDays * 2 else numMoveDays
    afterDelay(randomBelow(timeToMove) + 1)(moveRoom)
  }
}
