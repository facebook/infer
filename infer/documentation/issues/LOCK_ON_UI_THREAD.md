A method annoted as being on UIThread acquires a lock. This could be a potential performance issue

Example:

```java
class Example {
    @UiThread
    void foo() {
        synchronized(this) {
        }
    }
}
```
