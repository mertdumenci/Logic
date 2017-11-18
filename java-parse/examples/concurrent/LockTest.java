public class LockTest {
    public int lockTest(int a) {
        int i = 0;

        while (i < a) {
            Wunderhorn.lock();
            i += 1;
            Wunderhorn.lock();
            int c = 0;
            Wunderhorn.lock();
            c += 1;
            Wunderhorn.unlock();
            Wunderhorn.unlock();
            Wunderhorn.unlock();
        }

        return i;
    }
}