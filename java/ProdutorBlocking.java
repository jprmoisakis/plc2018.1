package semaforo;

import java.util.concurrent.BlockingQueue;

public class ProdutorBlocking implements Runnable {
	private BlockingQueue<Integer> queue;
	private int number;
	
	public ProdutorBlocking(BlockingQueue<Integer> queue, int number) {
		this.number = number;
		this.queue = queue;
	}
	
	@Override
	public void run() {
		for (int i = 0; i < 10; i++) {
			try {
				queue.put(i);
				System.out.println("Produtor " + number + " coloca: " + i);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
}
