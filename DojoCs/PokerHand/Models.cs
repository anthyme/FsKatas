using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DojoCs.PokerHand
{
    enum Suit { Spade, Heart, Club, Diamond }
    enum Rank { Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace }
    enum Combination { HighCard, OnePair, TwoPair, ThreeSame, Straight, Flush, FullHouse, FourSame, StraightFlush, RoyalStraightFlush }

    struct Card
    {
        public Rank Rank { get; }
        public Suit Suit { get; }

        public Card(Rank rank, Suit suit)
        {
            Suit = suit;
            Rank = rank;
        }
    }

    struct Hand
    {
        public HashSet<Card> Cards { get; }

        public Hand(IEnumerable<Card> cards)
        {
            Cards = new HashSet<Card>(cards);
        }

        public bool Equals(Hand other)
        {
            return Cards.SetEquals(other.Cards);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            return obj is Hand && Equals((Hand) obj);
        }

        public override int GetHashCode()
        {
            return Cards?.GetHashCode() ?? 0;
        }
    }
}