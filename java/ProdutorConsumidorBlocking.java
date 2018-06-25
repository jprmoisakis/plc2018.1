package semaforo;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

public class ProdutorConsumidorBlocking {
	public static void main(String[] args) {
		BlockingQueue<Integer> queue = new ArrayBlockingQueue<Integer>(1);
		ProdutorBlocking p = new ProdutorBlocking(queue, 1);
		ConsumidorBlocking c = new ConsumidorBlocking(queue, 1);
		Thread tp = new Thread(p);
		Thread tc = new Thread(c);
		tp.start();
		tc.start();
	}
}
