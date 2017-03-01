using System;
using System.Linq;

namespace DojoCs.PokerHand
{
    static class CombinationFinder
    {
        public static Combination FindCombination(Hand hand)
        {
            if (HasSameCount(hand, 3, 1) && HasSameCount(hand, 2, 1))
                return Combination.FullHouse;
            if (IsFlush(hand))
                return Combination.Flush;
            if (HasSameCount(hand, 3, 1))
                return Combination.ThreeSame;
            if (HasSameCount(hand, 2, 2))
                return Combination.TwoPair;
            if (HasSameCount(hand, 2, 1))
                return Combination.OnePair;
            return Combination.HighCard;
        }

        private static bool IsFlush(Hand hand)
        {
            return hand.Cards.GroupBy(x => x.Suit).Count() == 1;
        }

        private static bool HasSameCount(Hand hand, int numberOfElements, int numberOfGroup)
        {
            return hand.Cards.GroupBy(x => x.Rank).Count(g => g.Count() == numberOfElements) == numberOfGroup;
        }
    }

    static class PokerParser
    {
        public static Card ParseCard(string rawCard)
        {
            return new Card(ParseRank(rawCard[0]), ParseSuit(rawCard[1]));
        }

        public static Hand ParseHand(string rowHand)
        {
            return new Hand(rowHand.Split(' ').Select(ParseCard));
        }

        private static Suit ParseSuit(char rawSuit)
        {
            switch (rawSuit)
            {
                case 'h': return Suit.Heart;
                case 'c': return Suit.Club;
                case 'd': return Suit.Diamond;
                case 's': return Suit.Spade;
            }
            throw new ArgumentException(rawSuit + " unknown");
        }

        private static Rank ParseRank(char rawRank)
        {
            switch (rawRank)
            {
                case '2': return Rank.Two;
                case '3': return Rank.Three;
                case '4': return Rank.Four;
                case '5': return Rank.Five;
                case '6': return Rank.Six;
                case '7': return Rank.Seven;
                case '8': return Rank.Eight;
                case '9': return Rank.Nine;
                case 'T': return Rank.Ten;
                case 'J': return Rank.Jack;
                case 'Q': return Rank.Queen;
                case 'K': return Rank.King;
                case 'A': return Rank.Ace;
            }
            throw new ArgumentException(rawRank + " unknown");
        }
    }
}
