using NFluent;
using Xunit;

namespace DojoCs.PokerHand
{
    public class PokerTest
    {
        [Fact]
        public void TestTrue() => Check.That(true).Equals(true);
    }
}
