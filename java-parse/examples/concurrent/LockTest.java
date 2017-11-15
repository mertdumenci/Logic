public class LockTest {
    public int lockTest(int a) {
        int i = 0;

        while (i < a) {
            Wunderhorn.lock();
            i += 1;
            Wunderhorn.unlock();
        }

        return i;
    }
}