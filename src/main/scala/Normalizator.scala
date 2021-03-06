package fi.akisaarinen

object Normalizator {
  private def calculateTotalWeight(weight: List[Int]) = {
    math.sqrt(int2double(weight.map(x => {x * x}).sum))
  }

  def createNormalizedItems(items: List[ContentsItem]) : List[NormalizedContentsItem] = {
    val maxValue = int2double(items.reduceLeft( (x, y) => { if (y.value > x.value) y else x } ).value)
    val maxTotalWeight = calculateTotalWeight(items.reduceLeft( (x, y) => { if (calculateTotalWeight(y.weight) > calculateTotalWeight(x.weight)) y else x } ).weight)

    items.map( x => NormalizedContentsItem(x, calculateTotalWeight(x.weight) / maxTotalWeight, x.value / maxValue) )
  }

  def createNormalizedItems(knapsack : List[ContentsItem], leftovers : List[ContentsItem]) : (List[NormalizedContentsItem], List[NormalizedContentsItem]) = {
    val knapsackSize = knapsack.size
    val output = createNormalizedItems(knapsack ::: leftovers)
    output.splitAt(knapsack.size)
  }
}

object Simplifier {
  private def calculateTotalWeight(weight: List[Int]) = {
    math.sqrt(int2double(weight.map(x => {x * x}).sum))
  }

  def createSimplifiedItems(input : List[ContentsItem]) : List[NormalizedContentsItem] = {
    input.map( x => NormalizedContentsItem(x, calculateTotalWeight(x.weight), x.value) )
  }

  def createSimplifiedItems(knapsack : List[ContentsItem], leftovers : List[ContentsItem]) : (List[NormalizedContentsItem], List[NormalizedContentsItem]) = {
    val knapsackSize = knapsack.size
    val output = createSimplifiedItems(knapsack ::: leftovers)
    output.splitAt(knapsack.size)
  }
}