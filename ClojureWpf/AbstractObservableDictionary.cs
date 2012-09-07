using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;

namespace ClojureWpf
{
    public abstract class AbstractNotifyCollectionChanged : INotifyCollectionChanged, INotifyPropertyChanged
    {
        public void NotifyCollectionChanged(NotifyCollectionChangedEventArgs args)
        {
            var evt = CollectionChanged;
            if (evt != null) evt(this, args);
        }

        public void NotifyPropertyChanged(PropertyChangedEventArgs args)
        {
            var evt = PropertyChanged;
            if (evt != null) evt(this, args);
        }

        public virtual event NotifyCollectionChangedEventHandler CollectionChanged;
        public virtual event PropertyChangedEventHandler PropertyChanged;
    }

    public abstract class AbstractGenericObservableDictionary<K,V>
        : AbstractNotifyCollectionChanged, IDictionary<K, V>
    {
        public abstract IEnumerator<KeyValuePair<K, V>> GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public void Add(KeyValuePair<K, V> item)
        {
            Add(item.Key, item.Value);
        }

        public abstract void Clear();

        public virtual bool Contains(KeyValuePair<K, V> item)
        {
            V val = default(V);
            if(TryGetValue(item.Key, out val))
            {
                if(val.Equals(item.Value))
                {
                    return true;
                }
            }
            return false;
        }

        public virtual void CopyTo(KeyValuePair<K, V>[] array, int arrayIndex)
        {
            throw new NotSupportedException();
        }

        public virtual bool Remove(KeyValuePair<K, V> item)
        {
            
            V val = default(V);
            if(TryGetValue(item.Key, out val))
            {
                if(val.Equals(item.Value))
                {
                    Remove(item.Key);
                    return true;
                }
            }
            return false;
        }

        public abstract int Count { get; }
        public virtual bool IsReadOnly { get { return false; } }

        public abstract bool ContainsKey(K key);

        public virtual void Add(K key, V value)
        {
            if(ContainsKey(key))
                throw new ArgumentException();
            SetValue(key, value);
        }

        protected abstract void SetValue(K key, V value);

        public abstract bool Remove(K key);

        public abstract bool TryGetValue(K key, out V value);

        public V this[K key]
        {
            get
            {
                V value;
                if (TryGetValue(key, out value))
                    return value;
                throw new KeyNotFoundException();
            }
            set { SetValue(key, value); }
        }

        public abstract ICollection<K> Keys { get; }
        public abstract ICollection<V> Values { get; }

        public override event NotifyCollectionChangedEventHandler CollectionChanged;
        public override event PropertyChangedEventHandler PropertyChanged;
    }

    public abstract class AbstractObservableDictionary :
        AbstractGenericObservableDictionary<object, object>
    {
        public abstract object GetValue(object key);
        public override bool TryGetValue(object key, out object value)
        {
            value = GetValue(key);
            return value != null;
        }
    }

    public abstract class AbstractGenericObservableList<T>
        : AbstractNotifyCollectionChanged
        , IList<T>
    {
        public abstract IEnumerator<T> GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public abstract void Add(T item);
        public abstract void Clear();
        public abstract bool Contains(T item);
        public virtual void CopyTo(T[] array, int arrayIndex)
        {
            throw new NotSupportedException();
        }
        public abstract bool Remove(T item);
        public abstract int Count { get; }
        public virtual bool IsReadOnly { get { return true; } }
        public abstract int IndexOf(T item);
        public abstract void Insert(int index, T item);
        public abstract void RemoveAt(int index);
        public abstract T this[int index] { get; set; }
    }

    public abstract class AbstractObservableList
        : AbstractGenericObservableList<object>
    {}
}