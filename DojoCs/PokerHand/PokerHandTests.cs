using System;
using System.Collections.Generic;
using NFluent;
using Xunit;
using static DojoCs.PokerHand.PokerParser;
using static DojoCs.PokerHand.CombinationFinder;

namespace DojoCs.PokerHand
{
    public class PokerTest
    {
        [Fact]
        public void TwoSameCardsShouldBeEqual() =>
            Check.That(new Card(Rank.Ace, Suit.Heart)).Equals(new Card(Rank.Ace, Suit.Heart));

        [Fact]
        public void TwoSameHandsShouldBeEqual()
        {
            Hand hand1 = new Hand(new List<Card>()
            {
                new Card(Rank.Ace, Suit.Club),
                new Card(Rank.Eight, Suit.Diamond),
                new Card(Rank.Jack, Suit.Heart),
                new Card(Rank.Four, Suit.Spade),
                new Card(Rank.Seven, Suit.Club)
            });
            Hand hand2 = new Hand(new List<Card>()
            {
                new Card(Rank.Jack, Suit.Heart),
                new Card(Rank.Ace, Suit.Club),
                new Card(Rank.Eight, Suit.Diamond),
                new Card(Rank.Four, Suit.Spade),
                new Card(Rank.Seven, Suit.Club)
            });
            Check.That(hand1).Equals(hand2);
        }

        [Fact]
        public void InputShouldConvertToCard()
        {
            Check.That(ParseCard("2h")).Equals(new Card(Rank.Two, Suit.Heart));
        }

        [Fact]
        public void InputShouldConvertToCard2()
        {
            Check.That(ParseCard("2h")).IsNotEqualTo(new Card(Rank.Ace, Suit.Heart));
        }

        [Fact]
        public void InputShouldConvertToHand()
        {
            Hand hand = new Hand(new List<Card>()
            {
                new Card(Rank.Eight, Suit.Diamond),
                new Card(Rank.Jack, Suit.Heart),
                new Card(Rank.Ace, Suit.Club),
                new Card(Rank.Four, Suit.Spade),
                new Card(Rank.Seven, Suit.Club)
            });
            Check.That(ParseHand("Ac 8d Jh 4s 7c")).Equals(hand);
        }

        [Fact]
        public void HandShouldBeHighCard()
        {
            Check.That(FindCombination(ParseHand("Ac 8d Jh 4s 7c"))).Equals(Combination.HighCard);
        }

        [Fact]
        public void HandShouldBeOnePair()
        {
            Check.That(FindCombination(ParseHand("Ac Ad Jh 4s 7c"))).Equals(Combination.OnePair);
        }

        [Fact]
        public void HandShouldBeTwoPair()
        {
            Check.That(FindCombination(ParseHand("Ac Ad Jh Jc 7c"))).Equals(Combination.TwoPair);
        }

        [Fact]
        public void HandShouldBeThreeSame()
        {
            Check.That(FindCombination(ParseHand("Ac Jd Jh Jc 7c"))).Equals(Combination.ThreeSame);
        }

        [Fact]
        public void HandShouldBeFlush()
        {
            Check.That(FindCombination(ParseHand("2c 4c 7c Jc 8c"))).Equals(Combination.Flush);
        }

        [Fact]
        public void HandShouldBeFullHouse()
        {
            Check.That(FindCombination(ParseHand("Ac Jd Jh Jc Ah"))).Equals(Combination.FullHouse);
        } 
    }
}
