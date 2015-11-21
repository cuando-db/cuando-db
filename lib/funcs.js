db.system.js.save({
  _id: "map",
  value: function() { emit(this._id, filter(this, timeIntervalFilter)); }
})

db.system.js.save({
  _id: "reduce",
  value: function(k,v) { return null; }
})


db.system.js.save({
  _id: "isArray",
  value: function isArray(o) { return Array.isArray(o) || Object.prototype.toString.call(o) == '[object Array]'; }
})

db.system.js.save({
  _id: "isObject",
  value:
function isObject(o) {
  return o != null && typeof o === 'object' && !isArray(o);
}
})

db.system.js.save({
  _id: "isObjectObject",
  value:
function isObjectObject(o) {
  return isObject(o) === true
    && Object.prototype.toString.call(o) === '[object Object]';
}
})

db.system.js.save({
  _id: "isPlainObject",
  value:
function isPlainObject(o) {
  var ctor,prot;
  
  if (isObjectObject(o) === false) return false;
  
  ctor = o.constructor;
  if (typeof ctor !== 'function') return false;
  
  prot = ctor.prototype;
  if (isObjectObject(prot) === false) return false;
  
  if (prot.hasOwnProperty('isPrototypeOf') === false) {
    return false;
  }
  
  return true;
}
})

db.system.js.save({
  _id: "isCollection",
  value:
function isCollection(value) {
    return Array.isArray(value) || isPlainObject(value);
}
})

db.system.js.save({
  _id: "filter",
  value:
function filter(value, fn) {
    if (Array.isArray(value)) {
        return filterArray(value, fn);
    } else if (isObject(value)) {
        return filterObject(value, fn);
    }

    return value;
}
})

db.system.js.save({
  _id: "filterObject",
  value:
function filterObject(obj, fn) {
    var newObj = {};
    var key;
    var value;

    for (key in obj) {
        value = filter(obj[key], fn);

        if (fn.call(obj, value, key, obj)) {
            if (value !== obj[key] && !isCollection(value)) {
                value = obj[key];
            }

            newObj[key] = value;
        }
    }

    return newObj;
}
})

db.system.js.save({
  _id: "filterArray",
  value:
function filterArray(array, fn) {
    var filtered = [];

    array.forEach(function (value, index, array) {
        value = filter(value, fn);

        if (fn.call(array, value, index, array)) {
            if (value !== array[index] && !isCollection(value)) {
                value = array[index];
            }

            filtered.push(value);
        }
    });

    return filtered;
}
})


db.system.js.save({
  _id: "inTimeInterval",
  value: function (interval, start, finish) { return interval.start >= start && interval.finish <= finish; } 
})

db.system.js.save({
  _id: "timeIntervalFilter",
  value: function (val, prop, subj) {
  print(dimension, start, finish)
  if (isObject(val)) {
    if (val.hasOwnProperty(dimension)) {
      return inTimeInterval(val[dimension], start, finish);
    }
    else if (val.hasOwnProperty("context")) {
      return val.context.length > 0;
    }
  }
  return true;
}
})
