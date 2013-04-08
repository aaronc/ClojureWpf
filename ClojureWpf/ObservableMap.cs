using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Windows.Threading;
using clojure.lang;

namespace ClojureWpf
{
    public class ObservableMap
        : ObservableDictionary<string, object>
          ,ITransientMap
    {
        private Dispatcher dispatcher;

        public ObservableMap()
        {
            dispatcher = Dispatcher.CurrentDispatcher;
        }

        protected override void OnCollectionChanged(NotifyCollectionChangedEventArgs args)
        {
            if (dispatcher.CheckAccess())
                base.OnCollectionChanged(args);
            else
                dispatcher.BeginInvoke(new Action(() => base.OnCollectionChanged(args)));
        }

        public ITransientCollection conj(object val)
        {
            // Copied from ATransientMap.conj
            {
                IMapEntry e = val as IMapEntry;
                if (e != null)
                    return assoc(e.key(), e.val());
            }

            if (val is DictionaryEntry)
            {
                DictionaryEntry de = (DictionaryEntry)val;
                return assoc(de.Key, de.Value);
            }

            {
                IPersistentVector v = val as IPersistentVector;
                if (v != null)
                {
                    if (v.count() != 2)
                        throw new ArgumentException("Vector arg to map conj must be a pair");
                    return assoc(v.nth(0), v.nth(1));
                }
            }

            // TODO: also handle KeyValuePair?
            ITransientMap ret = this;
            for (ISeq es = RT.seq(val); es != null; es = es.next())
            {
                IMapEntry e = (IMapEntry)es.first();
                ret = ret.assoc(e.key(), e.val());
            }
            return ret;
        }

        private string TryGetKeyString(object key)
        {
            if (key is string) return (string) key;
            var keyword = key as Named;
            if (keyword == null)
                throw new ArgumentException("Only Keyword type or String type supported.");
            return keyword.getName();
        }

        public IPersistentMap persistent()
        {
            var hashMap = new Dictionary<object, object>();
            foreach (KeyValuePair<string, object> kvp in this)
            {
                hashMap.Add(Keyword.intern(kvp.Key), kvp.Value);
            }
            return PersistentHashMap.create(hashMap);
        }

        public ITransientMap assoc(object key, object val)
        {
            this[TryGetKeyString(key)] = val;
            return this;
        }

        public ITransientMap without(object key)
        {
            Remove(TryGetKeyString(key));
            return this;
        }

        IPersistentCollection ITransientCollection.persistent()
        {
            return persistent();
        }

        public object valAt(object key)
        {
            return valAt(TryGetKeyString(key), null);
        }

        public object valAt(object key, object notFound)
        {
            object res;
            if (TryGetValue(TryGetKeyString(key), out res))
                return res;
            return notFound;
        }

        ITransientAssociative ITransientAssociative.assoc(object key, object val)
        {
            return assoc(key, val);
        }

        public int count()
        {
            return Count;
        }

        public Dispatcher Dispatcher
        {
            get { return dispatcher; }
            set { dispatcher = value; }
        }
    }
}