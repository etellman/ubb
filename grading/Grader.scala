object Grader {
    /**
     * Creates a grading function for a homework set.
     *
     * @problems the problems in the assignment, probably copied from the latex file.
     * @param delta - the number of points missed (or extra credit) for this student
     * @return the score as a fraction and percentage
     * @throws IllegalArgumentException if the number of points is not positive
     */
    def forProblems(problems: Int*)(delta: Int): (String, String) = {
        val totalPoints: Int = points(problems.toList)

        forPoints(totalPoints)(delta)
    }

    /**
     * Creates a grading function for a specified number of points.
     *
     * @param numPoints - the total number of points
     * @param delta - the number of points missed (or extra credit) for this student
     * @return the score as a fraction and percentage
     * @throws IllegalArgumentException if the number of points is not positive
     */
    def forPoints(numPoints: Int)(delta: Int): (String, String) = {
        require(numPoints > 0, "non-positive number of points")

        val totalPoints = numPoints.toFloat
        val pointsEarned = numPoints + delta
        val percentage = pointsEarned.toFloat / numPoints.toFloat

        (pointsEarned.toString + '/' + numPoints.toString, (100.0 * percentage).round.toString + '%')
    } 

    /**
     * Calculates the number of points in a list of numbers copied from the assignment.  The original list probably
     * looked something like: 1-5, 7, 9, 10-11, etc.
     *
     * @param problems - the problem list
     * @return the number of points for this set of problems, where each problem is worth five points
     */
    def points(problems: List[Int]): Int = {
        val problemCounts = for (problem <- problems) yield {
            if (problem > 0) {
                1
            } else {
                - problem + 1
            }
        }

        problemCounts.map(_ * 5).sum
    } ensuring { _ >= 0 }
}

// a grader for the current homework set
def grade = Grader.forProblems(1-25, 31-32, 35-36, 38, 41-44, 51-52, 65-68, 72, 74, 76, 78)_

