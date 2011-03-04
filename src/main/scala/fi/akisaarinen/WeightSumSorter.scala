package fi.akisaarinen

class WeightSumSorter extends Algorithm {
  private def importance(x: ContentsItem) = x.value / (x.weight.sum)

  def sort(input : List[ContentsItem]) : List[ContentsItem] = {
    input.sortWith((x, y) => { importance(x) > importance(y) })
  }
}