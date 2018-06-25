package semaforo;

import java.util.concurrent.BlockingQueue;

public class ConsumidorBlocking implements Runnable {
	private BlockingQueue<Integer> queue;
	private int number;
	
	public ConsumidorBlocking(BlockingQueue<Integer> queue, int number) {
		this.queue = queue;
		this.number = number;
	}
	
	public void run() {
		int valor = 0;
		for (int i = 0; i < 10; i++) {
			try {
				valor = queue.take();
				System.out.println("Consumidor " + number + " pega: " + valor);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
}
