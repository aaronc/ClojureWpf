using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Windows.Threading;
using clojure.lang;

namespace ClojureWpf
{
    public class ObservableVector 
        : ObservableCollection<object>
        , ITransientVector
    {
        public ObservableVector()
        {
            Dispatcher = Dispatcher.CurrentDispatcher;
        }

        public Dispatcher Dispatcher { get; set; }

        public ITransientCollection conj(object val)
        {
            Add(val);
            return this;
        }

        public IPersistentCollection persistent()
        {
            return PersistentVector.create1(this);
        }

        public object valAt(object key)
        {
            return valAt(key, null);
        }

        protected override void OnCollectionChanged(NotifyCollectionChangedEventArgs args)
        {
            if (Dispatcher.CheckAccess())
                base.OnCollectionChanged(args);
            else
                Dispatcher.BeginInvoke(new Action(() => base.OnCollectionChanged(args)));
        }

        protected override void OnPropertyChanged(PropertyChangedEventArgs e)
        {
            if (Dispatcher.CheckAccess())
                base.OnPropertyChanged(e);
            else
                Dispatcher.BeginInvoke(new Action(() => base.OnPropertyChanged(e)));
        }

        public object valAt(object key, object notFound)
        {
            if (Util.IsNumeric(key))
            {
                var i = Util.ConvertToInt(key);
                if (i >= 0 && i < count())
                    return nth(i);
            }
            return notFound;
        }

        public ITransientAssociative assoc(object key, object val)
        {
            if (Util.IsNumeric(key))
            {
                var i = Util.ConvertToInt(key);
                if (i >= 0 && i < count())
                    return assocN(i, val);
            }
            throw new ArgumentException("Key must be an integer.");
        }

        public int count()
        {
            return Count;
        }

        public object nth(int i)
        {
            return this[i];
        }

        public object nth(int i, object notFound)
        {
            if (i >= 0 && i < count())
                return nth(i);
            return notFound;
        }

        public ITransientVector assocN(int i, object val)
        {
            this[i] = val;
            return this;
        }

        public ITransientVector pop()
        {
            if (Count > 0) 
                RemoveAt(Count - 1);
            return this;
        }
    }
}