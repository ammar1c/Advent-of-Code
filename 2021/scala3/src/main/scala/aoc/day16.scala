package aoc

object day16 {
  import Integer.{parseInt, toBinaryString}
  enum Packet:
    case Literal(version: Int, id: Int, literal: Long)
    case Operator(version: Int, id: Int, packets: Seq[Packet])

  import Packet._
  def parse(binStr: String): (Int, Packet) =
    val (version, id, packetLenId) = (parseInt(binStr.take(3), 2), parseInt(binStr.slice(3, 6), 2), binStr(6))
    (id, packetLenId) match
      case (4, _) =>
        var (i, last, literal) = (6, false, 0L)
        while (!last)
          if binStr(i) == '0' then last = true
          i += 1
          literal = literal << 4 | parseInt(binStr.slice(i, i + 4), 2)
          i += 4
        end while
        (i, Literal(version, id, literal))
      case (_, '0') =>
        val subsLen = parseInt(binStr.slice(7, 22), 2)
        var (i, limit) = (22, 22 + subsLen)
        var subpackets = List.empty[Packet]
        while (i < limit) {
          val (subLen, sub) = parse(binStr.substring(i))
          subpackets = sub :: subpackets
          i += subLen
        }
        (i, Operator(version, id, subpackets.reverse))
      case (_, '1') =>
        val subsCount = parseInt(binStr.slice(7, 18), 2)
        var i = 18
        var subpackets = List.empty[Packet]
        (0 until subsCount).foreach { _ =>
          val (subLen, sub) = parse(binStr.substring(i))
          subpackets = sub :: subpackets
          i += subLen
        }
        (i, Operator(version, id, subpackets.reverse))

  def sumVersions(packet: Packet): Long = packet match
    case Literal(version, _, _) => version
    case Operator(version, _, packets) => version + packets.map(sumVersions).sum

  def calcValue(packet: Packet): Long = packet match
    case Literal(_, _, literal) => literal
    case Operator(_, 0, packets) => packets.map(calcValue).sum
    case Operator(_, 1, packets) => packets.map(calcValue).product
    case Operator(_, 2, packets) => packets.map(calcValue).min
    case Operator(_, 3, packets) => packets.map(calcValue).max
    case Operator(_, 5, packets) => packets.map(calcValue).reduce((a, b) => if a > b then 1 else 0)
    case Operator(_, 6, packets) => packets.map(calcValue).reduce((a, b) => if a < b then 1 else 0)
    case Operator(_, _, packets) =>
      packets.map(calcValue) match
        case head :: rest =>
          rest.forall(_ == head) match
            case true => 1
            case false => 0


  def toBin(hexString: String): String  =
    hexString.flatMap(bin(_))

  def bin(c: Char): String =
    val fromHex = Seq("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100",
    "1101", "1110", "1111")
    return fromHex(if c.isDigit then c-'0' else c-'A'+10)


  def main(args: Array[String]): Unit = {
    val binS = toBin("C20D42002ED333E7774EAAC3C2009670015692C61B65892239803536C53E2D307A600ACF324928380133D18361005B336D3600B4BF96E4A59FED94C029981C96409C4A964F995D2015DE6BD1C6E7256B004C0010A86B06A1F0002151AE0CC79866600ACC5CABC0151006238C46858200E4178F90F663FBA4FDEC0610096F8019B8803F19A1641C100722E4368C3351D0E9802D60084DC752739B8EA4ED377DE454C0119BBAFE80213F68CDC66A349B0B0053B23DDD61FF22CB874AD1C4C0139CA29580230A216C9FF54AD25A193002A2FA002AB3A63377C124205008A05CB4B66B24F33E06E014CF9CCDC3A2F22B72548E842721A573005E6E5F76D0042676BB33B5F8C46008F8023301B3F59E1464FB88DCBE6680F34C8C0115CDAA48F5EE45E278380019F9EC6395F6BE404016849E39DE2EF002013C873C8A401544EB2E002FF3D51B9CAF03C0010793E0344D00104E7611C284F5B2A10626776F785E6BD672200D3A801A798964E6671A3E9AF42A38400EF4C88CC32C24933B1006E7AC2F3E8728C8E008C759B45400B4A0B4A6CD23C4AF09646786B70028C00C002E6D00AEC1003440080024658086A401EE98070B50029400C0014FD00489000F7D400E000A60001E870038800AB9AB871005B12B37DB004266FC28988E52080462973DD0050401A8351DA0B00021D1B220C1E0013A0C0198410BE1C180370C21CC552004222FC1983A0018FCE2ACBDF109F76393751D965E3004E763DB4E169E436C0151007A10C20884000874698630708050C00043E24C188CC0008744A8311E4401D8B109A3290060BE00ACEA449214CD7B084B04F1A48025F8BD800AB4D64426B22CA00FC9BE4EA2C9EA6DC40181E802B39E009CB5B87539DD864A537DA7858C011B005E633E9F6EA133FA78EE53B7DE80")

    val (len, packet) = parse(binS)

    println(sumVersions(packet))
    println(calcValue(packet))
  }
}
