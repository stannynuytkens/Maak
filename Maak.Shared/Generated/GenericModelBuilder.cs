using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq.Expressions;
using Bogus;

namespace Maak.Shared
{
	[ExcludeFromCodeCoverage]
    public class GenericModelBuilder<T> where T : class
    {
        private readonly Faker _faker = new Faker();

        protected Faker<T> EntityFaker { get; }

        public GenericModelBuilder()
        {
            EntityFaker = new Faker<T>();
        }

        public GenericModelBuilder<T> SetDefaultRules(Action<Faker, T> rules)
        {
            EntityFaker.Rules(rules);
            return this;
        }

        public GenericModelBuilder<T> With<TProp>(Expression<Func<T, TProp>> expression, Func<TProp> value)
        {
            EntityFaker.RuleFor(expression, value);
            return this;
        }

        public GenericModelBuilder<T> With<TProp>(Expression<Func<T, TProp>> expression, TProp value)
        {
            EntityFaker.RuleFor(expression, value);
            return this;
        }

        public GenericModelBuilder<T> With<TProp>(Expression<Func<T, TProp>> expression, Func<Faker, TProp> faker)
        {
            EntityFaker.RuleFor(expression, faker(_faker));
            return this;
        }

        public virtual T Build()  => EntityFaker.Generate();

        public virtual IList<T> BuildList(int count) => EntityFaker.Generate(count);
    }
}